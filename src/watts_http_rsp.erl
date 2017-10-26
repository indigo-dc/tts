%% @doc the http endpoint to redirect a user to for an RSP flow.
-module(watts_http_rsp).
-behaviour(cowboy_http_handler).
-export([
         init/3,
         terminate/3,
         handle/2]).

-include("watts.hrl").

-type state() :: state.

%% @doc init the request handling, does nothing
-spec init(any(), cowboy_req:req(), any()) -> {ok, cowboy_req:req(), state()}.
init(_Type, Req, _Opts) ->
    {ok, Req, state}.

%% @doc on termination do nothing
-spec terminate(any(), cowboy_req:req(),  state()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%% @doc start a session if jwt parsing succeeded, else retrun back
-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), []}.
handle(Req, _State) ->
    {Path, Req2} = cowboy_req:path(Req),
    {Referer, Req3} = cowboy_req:header(<<"referer">>, Req2),
    JwtData = lists:last(binary:split(Path, <<"/">>, [global, trim_all])),
    Result = watts_rsp:validate_jwt_get_rsp(JwtData, Referer),
    setup_session_and_start(Result, Req3).

%% @doc check the parsing result and maybe start the session and trigger
%% the needed action.
-spec setup_session_and_start({ok, watts_rsp:rsp()} | {error, any(), binary()},
                              cowboy_req:req()) ->  {ok, cowboy_req:req(), []}.
setup_session_and_start({ok, Rsp}, Req) ->
    {ok, Session} = watts:session_for_rsp(Rsp),
    {ok, Req2} = set_cookie(Session, Req),
    execute_or_error(watts_rsp:request_type(Rsp), Session, Rsp, Req2);
setup_session_and_start({error, Error, ReturnUrl}, Req) ->
    warning("~p", [Error]),
    watts_http_util:redirect_to(ReturnUrl, Req).

%% @doc redirect to the provider for login or trigger the plugin
-spec execute_or_error(watts_rsp:request_type() | {error, Reason :: atom()},
                       pid(), watts_rsp:rsp(), cowboy_req:req())
                      -> {ok, cowboy_req:req(), []}.
execute_or_error(rsp_no_ui_no_login, Session, Rsp, Req) ->
    {ServiceId, Params} = watts_rsp:get_service_data(Rsp),
    Result = watts:request_credential_for(ServiceId, Session, Params),
    watts_http_util:redirect_to(get_return_url(Result, Rsp), Req);
execute_or_error(rsp_ui_no_login, _Session, _Rsp, Req) ->
    Url = watts_http_util:relative_path(""),
    watts_http_util:redirect_to(Url, Req);
execute_or_error(rsp_no_ui_login, Session, Rsp, Req) ->
    redirect_to_provider(Session, Rsp, Req);
execute_or_error(rsp_ui_login, Session, Rsp, Req) ->
    redirect_to_provider(Session, Rsp, Req);
execute_or_error({error, Reason}, Session, Rsp, Req) ->
    % bad request type
    warning("~p", [Reason]),
    watts:logout(Session),
    watts_http_util:redirect_to(get_return_url(failed, Rsp), Req).

%% @doc return the url to use on fail/success of plugin execution
-spec get_return_url({ok, any()} | any(), watts_rsp:rsp()) -> binary().
get_return_url({ok, _}, Rsp) ->
    {Url, _} = watts_rsp:get_return_urls(Rsp),
    Url;
get_return_url(_, Rsp) ->
    {_, Url} = watts_rsp:get_return_urls(Rsp),
    Url.

%% @doc redirect the a provider for login
-spec redirect_to_provider(pid(), watts_rsp:rsp(), cowboy_req:req())
                          -> {ok, cowboy_req:req(), []}.
redirect_to_provider(Session, Rsp, Req) ->
    {ok, Req2} = set_cookie(Session, Req),
    Provider = watts_rsp:get_provider(Rsp),
    Path = io_lib:format("oidc?provider=~s", [binary_to_list(Provider)]),
    Url = watts_http_util:relative_path(lists:flatten(Path)),
    watts_http_util:redirect_to(Url, Req2).

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
