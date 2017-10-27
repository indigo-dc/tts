%% @doc the http endpoint to redirect a user to for an RSP flow.
-module(watts_http_rsp).
-behaviour(cowboy_http_handler).
-export([
         init/3,
         terminate/3,
         handle/2]).

-include("watts.hrl").

-record(state, {
          token = undefined
         }).

-type state() :: #state{}.

%% @doc init the request handling, does nothing
-spec init(any(), cowboy_req:req(), any()) -> {ok, cowboy_req:req(), state()}.
init(_Type, Req, _Opts) ->
    {ok, Req, #state{}}.

%% @doc on termination do nothing
-spec terminate(any(), cowboy_req:req(),  state()) -> ok.
terminate(_Reason, _Req, #state{token = undefined}) ->
    ok;
terminate(_Reason, _Req, #state{token = Token}) ->
    jobs:done(Token),
    ok.

%% @doc start a session if jwt parsing succeeded and get a queue entry,
%% else retrun back
-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State) ->
    QueueUsed = ?CONFIG(watts_rsp_queue, undefined),
    {Result, NewState} = request_queue_if_configured(QueueUsed, State),
    maybe_handle(Result, Req, NewState).

%% @doc maybe handle the request, if there are not too many per second.
-spec maybe_handle(boolean(), cowboy_req:req(), state())
                  -> {ok, cowboy_req:req(), state()}.
maybe_handle(true, Req, State) ->
    {Path, Req2} = cowboy_req:path(Req),
    {Referer, Req3} = cowboy_req:header(<<"referer">>, Req2),
    JwtData = lists:last(binary:split(Path, <<"/">>, [global, trim_all])),
    Result = watts_rsp:validate_jwt_get_rsp(JwtData, Referer),
    setup_session_and_start(Result, Req3, State);
maybe_handle(false, Req, State) ->
    {ok, Req2} = cowboy_req:reply(503, Req),
    {ok, Req2, State}.

%% @doc request a token at the queue if needed.
-spec request_queue_if_configured(boolean(), state()) -> {boolean(), state()}.
request_queue_if_configured(true, State) ->
    Result = jobs:ask(watts_rsp_queue),
    handle_queue_result(Result, State);
request_queue_if_configured(_, State) ->
    {true, State}.

%% @doc handle the toke or error of requesting the queue
-spec handle_queue_result({ok, any()} | any(), state()) -> {boolean(), state()}.
handle_queue_result({ok, Token}, State) ->
    {true, State#state{token = Token}};
handle_queue_result(_, State) ->
    {false, State}.

%% @doc check the parsing result and maybe start the session and trigger
%% the needed action.
-spec setup_session_and_start({ok, watts_rsp:rsp()} | {error, any(), binary()},
                              cowboy_req:req(), state())
                             -> {ok, cowboy_req:req(), state()}.
setup_session_and_start({ok, Rsp}, Req, State) ->
    {ok, Session} = watts:session_for_rsp(Rsp),
    {ok, Req2} = set_cookie(Session, Req),
    execute_or_error(watts_rsp:request_type(Rsp), Session, Rsp, Req2, State);
setup_session_and_start({error, Error, ReturnUrl}, Req, State) ->
    warning("~p", [Error]),
    Req1 = watts_http_util:redirect_to(ReturnUrl, Req),
    {ok, Req1, State}.

%% @doc redirect to the provider for login or trigger the plugin
-spec execute_or_error(watts_rsp:request_type() | {error, Reason :: atom()},
                       pid(), watts_rsp:rsp(), cowboy_req:req(), state())
                      -> {ok, cowboy_req:req(), state()}.
execute_or_error(rsp_no_ui_no_login, Session, Rsp, Req, State) ->
    {ServiceId, Params} = watts_rsp:get_service_data(Rsp),
    Result = watts:request_credential_for(ServiceId, Session, Params),
    Req1 = watts_http_util:redirect_to(get_return_url(Result, Rsp), Req),
    {ok, Req1, State};
execute_or_error(rsp_ui_no_login, _Session, _Rsp, Req, State) ->
    Url = watts_http_util:relative_path(""),
    Req1 = watts_http_util:redirect_to(Url, Req),
    {ok, Req1, State};
execute_or_error(rsp_no_ui_login, Session, Rsp, Req, State) ->
    redirect_to_provider(Session, Rsp, Req, State);
execute_or_error(rsp_ui_login, Session, Rsp, Req, State) ->
    redirect_to_provider(Session, Rsp, Req, State);
execute_or_error({error, Reason}, Session, Rsp, Req, State) ->
    % bad request type
    warning("~p", [Reason]),
    watts:logout(Session),
    Req1 = watts_http_util:redirect_to(get_return_url(failed, Rsp), Req),
    {ok, Req1, State}.

%% @doc return the url to use on fail/success of plugin execution
-spec get_return_url({ok, any()} | any(), watts_rsp:rsp()) -> binary().
get_return_url({ok, _}, Rsp) ->
    {Url, _} = watts_rsp:get_return_urls(Rsp),
    Url;
get_return_url(_, Rsp) ->
    {_, Url} = watts_rsp:get_return_urls(Rsp),
    Url.

%% @doc redirect the a provider for login
-spec redirect_to_provider(pid(), watts_rsp:rsp(), cowboy_req:req(), state())
                          -> {ok, cowboy_req:req(), state()}.
redirect_to_provider(Session, Rsp, Req, State) ->
    {ok, Req2} = set_cookie(Session, Req),
    Provider = watts_rsp:get_provider(Rsp),
    Path = io_lib:format("oidc?provider=~s", [binary_to_list(Provider)]),
    Url = watts_http_util:relative_path(lists:flatten(Path)),
    Req3 = watts_http_util:redirect_to(Url, Req2),
    {ok, Req3, State}.


%% @doc set the cookie for session.
-spec set_cookie(pid(), cowboy_req:req()) -> {ok, cowboy_req:req()}.
set_cookie(Session, Req) ->
    {ok, Max, Token} = watts_session_mgr:get_cookie_data(Session),
    watts_http_util:perform_cookie_action(update, Max, Token, Req).

%% @doc log a warning
-spec warning(string() | binary(), [any()]) -> ok.
warning(Msg, Params) ->
    Message = io_lib:format(Msg, Params),
    lager:warning("rsp request failed with ~s", [Message]).
