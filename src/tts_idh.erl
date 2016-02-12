-module(tts_idh).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([lookup_user/1]).

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
          config = #{}
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec lookup_user(Map :: map()) -> {ok, UserId :: list(), Info :: map()} | {error, Reason :: term}.
lookup_user(Map) ->
    gen_server:call(?MODULE, {lookup_user, Map}).

%% gen_server.

init([]) ->
	{ok, #state{},100}.

handle_call({lookup_user, Map}, _From, #state{type=Type}=State) ->
    {Result, NewState} = perform_lookup(Type, Map, State),
    {reply, Result, NewState};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout, State) ->
    update_config(State);
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


perform_lookup(ldap, Map, State) ->
    perform_ldap_lookup(Map,State);
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
    Result = convert_ldap_result(LdapResult),
    {Result, State}.

create_ldap_filter(#{iss := BinIssuer, sub := BinSubject} ) ->
    BinIndigoId = << BinIssuer/binary, <<" ">>/binary, BinSubject/binary >>,
    IndigoId = binary:bin_to_list(BinIndigoId),
    eldap:equalityMatch("indigoId",IndigoId). 

convert_ldap_result({ok,#eldap_search_result{entries = []}}) ->
    {error, not_found};
convert_ldap_result({ok,#eldap_search_result{entries =
                                             [#eldap_entry{object_name=Name,
                                                           attributes=Attributes}]}}) ->
    ToAtom = fun(Key, Value, Map) ->
                     AKey = list_to_atom(Key),
                     maps:put(AKey,Value,Map)
             end,
    UserInfo = maps:fold(ToAtom,#{},maps:from_list(Attributes)),
    {ok, Name, UserInfo};
convert_ldap_result(_Result) ->
    {error, not_supported}.


update_config(#state{type = undefined} = State) ->
   set_config(config_exists(),State);
update_config(State) ->
    {noreply,State}.


set_config(true,_State) ->
    Config = generate_config(?CONFIG(idh_type)),
    State = #state{
               type = ?CONFIG(idh_type),
               config = Config
              },
    {noreply,State};
set_config(false,State) ->
    {noreply,State,60000}.

config_exists() ->
    case ?CONFIG(idh_type,undefined) of
        undefined -> false;
        _ -> true
    end.

generate_config(ldap) ->
    #{ host => ?CONFIG(idh_host),
       port => ?CONFIG(idh_port),
       user => ?CONFIG(idh_user),
       passwd => ?CONFIG(idh_passwd),
       base => ?CONFIG(idh_base)
     };
generate_config(_) ->
    #{}.
