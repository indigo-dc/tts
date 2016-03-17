-module(tts_idh).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([lookup_user/1]).
-export([update_config/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("tts.hrl").
-include_lib("eldap/include/eldap.hrl").

-record(state, {
          type = undefined,
          configured = false,
          config = #{}
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec lookup_user(Map :: map()) -> {ok, Info :: map()} | {error, Reason :: term}.
lookup_user(Map) ->
    gen_server:call(?MODULE, {lookup_user, Map}).

-spec update_config() -> ok.
update_config() ->
	gen_server:call(?MODULE, update_config).
%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call(update_config, _From, State) ->
    {Result, NewState} = update_config(State),
    {reply, Result, NewState};
handle_call({lookup_user, _Map}, _From, #state{configured=false}=State) ->
    {reply,{error, not_configured} , State};
handle_call({lookup_user, Map}, _From, #state{type=Type}=State) ->
    {Result, NewState} = perform_lookup(Type, Map, State),
    {reply, Result, NewState};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


perform_lookup(ldap, Map, State) ->
    perform_ldap_lookup(Map,State);
perform_lookup(file,Map,State) ->
    perform_file_lookup(Map,State);
perform_lookup(_,_,State) ->
    {{error, not_supported}, State}.


perform_ldap_lookup(Map, #state{config=Config} = State) ->
    %TODO: change into own worker 
    #{ host := Host,
       port := Port,
       user := User,
       passwd := Pass,
       base := Base
     } = Config,
    Filter = create_ldap_filter(Map),
    {ok, Handle} = eldap:open([Host],[{port,Port}]),
    ok = eldap:simple_bind(Handle,User,Pass),
    LdapResult = eldap:search(Handle,[{base, Base}, 
                                  {filter, Filter},
                                  {attributes,["uid","uidNumber","gidNumber","homeDirectory"]}]),
    ok = eldap:close(Handle),
    Result = convert_ldap_result(LdapResult,Map),
    {Result, State}.

create_ldap_filter(#{iss := BinIssuer, sub := BinSubject} ) ->
    BinIndigoId = << BinIssuer/binary, <<" ">>/binary, BinSubject/binary >>,
    IndigoId = binary:bin_to_list(BinIndigoId),
    eldap:equalityMatch("indigoId",IndigoId). 

convert_ldap_result({ok,#eldap_search_result{entries = []}},_Map) ->
    {error, not_found};
convert_ldap_result({ok,#eldap_search_result{entries =
                                             [#eldap_entry{
                                                 attributes=Attributes}]}},
                    #{iss := Issuer, sub := Subject}) ->
    ToAtom = fun(Key, Value, Map) ->
                     AKey = list_to_atom(Key),
                     maps:put(AKey,Value,Map)
             end,
    UserInfo = maps:fold(ToAtom,#{},maps:from_list(Attributes)),
    UserInfo2 = maps:put(user_ids,[{Issuer,Subject}],UserInfo),
    {ok, UserInfo2};
convert_ldap_result(_Result,_Map) ->
    {error, not_supported}.


perform_file_lookup(#{iss := Iss, sub := Sub}, #state{config=Config} = State) ->
    #{ mappings := Mappings, users := Users } = Config,
    case lists:keyfind({Iss, Sub},1,Mappings) of
        false -> {{error, no_mapping},State};
        {_, Uid} -> case lists:keyfind(Uid,1,Users) of
                         false -> {{error, user_not_found},State};
                         E -> Result = data_entry_to_result(E,Mappings),
                             {Result, State}
                     end
    end.

data_entry_to_result({Uid, UidNumber, GidNumber, HomeDir},Mappings) ->
    Uids = [ID || {ID, U} <- Mappings, U == Uid ],
    {ok, #{
            uid => Uid, 
            user_ids => Uids,
            uidNumber => UidNumber, 
            gidNumber => GidNumber, 
            homeDirectory => HomeDir}}.

update_config(State) ->
    Config = generate_config(?CONFIG(idh_type)),
    NewState = State#state{
               type = ?CONFIG(idh_type),
               config = Config
              },
    {ok,NewState}.


generate_config(ldap) ->
    #{ host => ?CONFIG(idh_host),
       port => ?CONFIG(idh_port),
       user => ?CONFIG(idh_user),
       passwd => ?CONFIG(idh_passwd),
       base => ?CONFIG(idh_base)
     };
generate_config(file) ->
    File = ?CONFIG(idh_file),
    {Mappings, Users} = parse_file(File),
    #{ file => File ,
       mappings => Mappings,
       users => Users
     };
generate_config(_) ->
    #{}.


parse_file(undefined) ->
    [];
parse_file(File) ->
    {ok, IoDev} = file:open(File,[read, binary]),
    Line = file:read_line(IoDev),
    parse_lines(Line,IoDev,{[],[]}).


parse_lines(eof,IoDev,{Map, User}) ->
    file:close(IoDev),
    {lists:reverse(Map), lists:reverse(User)};
parse_lines({error,_},IoDev,_Tuple) ->
    file:close(IoDev),
    [];
parse_lines({ok,Line},IoDev,Tuple) ->
    NewLine = file:read_line(IoDev),
    NewList = parse_line(Line,Tuple),
    parse_lines(NewLine,IoDev,NewList).

parse_line(Line,Tuple) ->
    case jsx:is_json(Line) of
        true -> 
            Entry = jsx:decode(Line,[return_maps,{labels,attempt_atom}]),
            verify_and_add(Entry,Tuple);
        false ->
            Tuple 
    end.

verify_and_add(#{uid := Uid, uidNumber := UidNumber, gidNumber := GidNumber, homeDirectory := HomeDir}, 
               {Mappings, Users} ) when
      is_binary(Uid), is_integer(UidNumber),is_integer(GidNumber), is_binary(HomeDir) ->
    {Mappings, [{Uid, UidNumber, GidNumber, HomeDir} | Users]};
verify_and_add(#{iss := Iss, sub := Sub, uid := Uid}, {Mappings, Users}) when
      is_binary(Iss), is_binary(Sub), is_binary(Uid) ->
    {[{{Iss, Sub}, Uid} | Mappings], Users};
verify_and_add(_Entry, Tuple) ->
    Tuple.


