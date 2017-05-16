-module(watts_http_rsp).
-behaviour(cowboy_http_handler).
-export([
         init/3,
         terminate/3,
         handle/2]).

-include("watts.hrl").

init(_Type, Req, _Opts) ->
    {ok, Req, []}.

terminate(_Reason, _Req, _State) ->
    ok.

handle(Req, _State) ->
    {Path, Req2} = cowboy_req:path(Req),
    {Referer, Req3} = cowboy_req:header(<<"referer">>, Req2),
    JwtData = lists:last(binary:split(Path, <<"/">>, [global, trim_all])),
    Result = watts_rsp:validate_jwt_get_rsp(JwtData, Referer),
    setup_session_and_start(Result, Req3).

setup_session_and_start({ok, Rsp}, Req) ->
    {ok, Session} = watts:session_for_rsp(Rsp),
    {ok, Req2} = login_with_rsp(Session, Rsp, Req),
    execute_or_error(watts_rsp:request_type(Rsp), Session, Rsp, Req2);
setup_session_and_start(Error, Req) ->
    warning("~p", [Error]),
    {ok, Req2} = cowboy_req:reply(400, Req),
    {ok, Req2, []}.


execute_or_error(rsp_no_ui_no_login, Session, Rsp, Req) ->
    {ServiceId, Params} = watts_rsp:get_service_data(Rsp),
    watts:request_credential_for(ServiceId, Session, Params),
    Url = watts_rsp:get_return_url(Rsp),
    watts_http_util:redirect_to(Url, Req);
execute_or_error(rsp_ui_no_login, _Session, _Rsp, Req) ->
    Url = watts_http_util:relative_path(""),
    watts_http_util:redirect_to(Url, Req);
execute_or_error(rsp_no_ui_login, Session, Rsp, Req) ->
    redirect_to_provider(Session, Rsp, Req);
execute_or_error(rsp_ui_login, Session, Rsp, Req) ->
    redirect_to_provider(Session, Rsp, Req);
execute_or_error({error, Reason}, Session, _Rsp, Req) ->
    % bad request type
    warning("~p", [Reason]),
    watts:logout(Session),
    {ok, Req2} = cowboy_req:reply(400, Req),
    {ok, Req2, []}.

login_with_rsp(Session, Rsp, Req) ->
    watts:login_with_rsp(Rsp, Session),
    set_cookie(Session, Req).

redirect_to_provider(Session, Rsp, Req) ->
    {ok, Req2} = set_cookie(Session, Req),
    Provider = watts_rsp:get_provider(Rsp),
    Path = io_lib:format("oidc?provider=~s", [binary_to_list(Provider)]),
    Url = watts_http_util:relative_path(Path),
    watts_http_util:redirect_to(Url, Req2).

set_cookie(Session, Req) ->
    {ok, Max} = watts_session:get_max_age(Session),
    {ok, Token} = watts_session:get_sess_token(Session),
    watts_http_util:perform_cookie_action(update, Max, Token, Req).


warning(Msg, Params) ->
    Message = io_lib:format(Msg, Params),
    lager:warning("rsp request failed with ~s", [Message]).
