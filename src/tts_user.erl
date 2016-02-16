-module(tts_user).
-behaviour(gen_server).

-include("tts.hrl").

%% API.
-export([start_link/0]).
-export([set_user_info/2]).
-export([get_user_info/1]).
-export([get_credential_list/1]).
-export([connect_session/2]).
-export([add_credential/2]).
-export([shutdown/1]).


%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec set_user_info(UserInfo :: map(), Pid :: pid()) -> ok.
set_user_info(Info, Pid) ->
    gen_server:call(Pid,{set_user_info,Info}).

-spec get_user_info(Pid :: pid()) -> {ok, UserInfo :: map()}.
get_user_info(Pid) ->
    gen_server:call(Pid,get_user_info).

-spec get_credential_list(Pid :: pid()) -> {ok, CredentialList :: list()}.
get_credential_list(Pid) ->
    gen_server:call(Pid,get_credential_list).

-spec connect_session(Session :: pid(), Pid :: pid()) -> ok.
connect_session(Session, Pid) ->
    gen_server:call(Pid,{connect_session,Session}).

-spec add_credential(CredentialMap :: map(), Pid :: pid()) -> ok.
add_credential(CredentialMap, Pid) ->
    gen_server:call(Pid,{add_credential_map,CredentialMap}).


-spec shutdown(Pid :: pid()) -> ok.
shutdown(Pid) -> 
    gen_server:cast(Pid,shutdown).


%% gen_server.

-record(state, {
          uid = undefined,
          uidNumber = undefined,
          gidNumber = undefined,
          homeDirectory = undefined,
          user_ids = [],
          token = [],
          credentials = [],
          sessions = []
}).

init([]) ->
	{ok, #state{}, 5000}.

handle_call({set_user_info,UserInfo}, _From, State) ->
    #{ uid := Uid,
       uidNumber := UidNumber,
       gidNumber := GidNumber,
       homeDirectory := Dir,
       user_ids := UserIds
     } = UserInfo,
    NewState = State#state{ uid = Uid,
                            uidNumber = UidNumber,
                            gidNumber = GidNumber,
                            homeDirectory =Dir,
                            user_ids = UserIds
                          }, 

    {reply, ok, NewState};
handle_call(get_user_info, _From, State) ->
    #state{ uid = Uid,
            uidNumber = UidNumber,
            gidNumber = GidNumber,
            homeDirectory =Dir,
            user_ids = UserIds
          } = State, 

    UserInfo = #{ uid => Uid,
       uidNumber => UidNumber,
       gidNumber => GidNumber,
       homeDirectory => Dir,
       user_ids => UserIds
     },
    {reply, {ok, UserInfo}, State};
handle_call(get_credential_list, _From, #state{credentials=Creds}=State) ->
    {reply, {ok, Creds}, State};
handle_call({connect_session,Session}, _From,#state{sessions=Sessions} = State) ->
    MRef = monitor(process, Session),
    NewSessions = [ {Session, MRef} | lists:keydelete(Session,1,Sessions) ],
    {reply, ok, State#state{sessions=NewSessions}};
handle_call({add_credential_map,CredentialMap},_From,State) ->
    {ok, NewState} = add_credential_to_state(CredentialMap,State),
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', MonitorRef, process, _Object, _Info}, #state{sessions=Sessions}=State) ->
    NewSessions = remove_session_from_list(MonitorRef,Sessions),
    shutdown_if_done(NewSessions,State);
handle_info(timeout, State) ->
    ok = tts_user_mgr:user_wants_to_shutdown(self()),
    {noreply,State,5000};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

shutdown_if_done([],State) ->
    NewState = State#state{sessions=[]},
    case has_unexpired_token(State) of
        true ->
            {noreply,NewState};
        false ->
            {noreply, NewState, ?CONFIG(user_timeout)}
    end;
shutdown_if_done(Sessions,State) ->
    {noreply,State#state{sessions=Sessions}}.

has_unexpired_token(_State) ->
    %TODO: implement
    false.

add_credential_to_state(#{ token := T } = Cred, State) ->
    %TODO: change for keyfind will perform better
    #state{ token = TokenList, credentials = CredList} = State,
    NewTokenList = [T | TokenList],
    NewCredList = [Cred | CredList],
    NewState = State#state{
                    token = NewTokenList,
                    credentials = NewCredList
                },
    {ok, NewState}.

remove_session_from_list(MonitorRef,Sessions) ->
    lists:keydelete(MonitorRef,2,Sessions).

