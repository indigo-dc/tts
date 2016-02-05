-module(tts_user).
-behaviour(gen_server).

-include("tts.hrl").

%% API.
-export([start_link/1]).
-export([set_user_info/2]).
-export([get_user_info/1]).
-export([get_credential_list/1]).
-export([connect_session/2]).
-export([add_token/2]).


%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          sub = undefined,
          iss = undefined,
          user_info = #{},
          credentials = [],
          sessions = []
}).

%% API.

-spec start_link(User :: term()) -> {ok, pid()}.
start_link({Subject, Issuer}) ->
	gen_server:start_link(?MODULE, [Subject, Issuer], []).

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

-spec add_token(TokenMap :: map(), Pid :: pid()) -> ok.
add_token(TokenMap, Pid) ->
    gen_server:call(Pid,{add_token_map,TokenMap}).

%% gen_server.

init([Subject, Issuer]) ->
	{ok, #state{sub = Subject, iss = Issuer}, 5000}.

handle_call({set_user_info,UserInfo}, _From, State) ->
    {reply, ok, State#state{user_info=UserInfo}};
handle_call({connect_session,Session}, _From,#state{sessions=Sessions} = State) ->
    MRef = monitor(process, Session),
    NewSessions = [ {Session, MRef} | lists:keydelete(Session,1,Sessions) ],
    {reply, ok, State#state{sessions=NewSessions}};
handle_call({add_token_map,TokenMap}, _From, State) ->
    {ok, NewState} = update_token(TokenMap,State),
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', MonitorRef, process, _Object, _Info}, #state{sessions=Sessions}=State) ->
    NewSessions = remove_session_from_list(MonitorRef,Sessions),
    shutdown_if_done(NewSessions,State);
handle_info(timeout, State) ->
    {stop,normal,State};
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

update_token(_TokenMap, State) ->
    % TODO: implement
    {ok, State}.

remove_session_from_list(MonitorRef,Sessions) ->
    lists:keydelete(MonitorRef,2,Sessions).

