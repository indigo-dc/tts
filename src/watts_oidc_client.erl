
%% @doc the OpenID Connect client implementation for WaTTS.
%% This modules handles the
-module(watts_oidc_client).
-behaviour(oidcc_client).
-include("watts.hrl").

-export([
         login_succeeded/2,
         login_failed/3,

         login_succeeded/1,
         login_failed/2
        ]).

%% @doc handle the successful login of a user at an oidc provider.
%% This only handles logins via the java script web interface,
%% logins at the api using access tokens are handled directly at the api.
%% @see watts_http_api
-spec login_succeeded(TokenMap :: map(), EnvironmentMap :: map())
                     -> {ok, [tuple()]}.
login_succeeded(TokenMap, EnvMap) ->
    case watts:login_with_oidcc(TokenMap, get_session_type(EnvMap)) of
        {ok, #{session_pid := SessPid, session_type := {rsp, no_ui, login} }} ->
            {ok, #{service := Service,
                   params := Params
                  }} = watts_session:get_redirection(SessPid),
            Result = watts:request_credential_for(Service, SessPid, Params),
            redirect_back(Result, SessPid, EnvMap);
        {ok, #{session_pid := SessPid, session_type := {rsp, ui, login} }} ->
            redirect_set_cookie(SessPid, EnvMap);
        {ok, #{session_pid := SessPid, session_type := oidc }} ->
            redirect_set_cookie(SessPid, EnvMap);
        {error, Reason} ->
            lager:warning("login failed internal: ~p", [Reason]),
            ErrMsg = bin_error_msg(login, undefind),
            redirect_error(ErrMsg, EnvMap)
    end.

%% @doc handles a failed login of a user at the web interface (java script ui).
%% @see login_succeeded/2
-spec login_failed(Reason :: atom(), Details :: any(), EnvironmentMap :: map())
                  -> {ok, [tuple()]}.
login_failed(Reason, Details, EnvMap) ->
    lager:warning("login failed: ~p - ~p", [Reason, Details]),
    {SessType, Sess} = get_session_type(EnvMap),
    handle_failed_for_session(SessType, Sess, EnvMap, Reason, Details).

%% @doc helper function to generate a redirect depending of the requests origin.
-spec handle_failed_for_session(SessionType :: any(), pid(), map(), atom(),
                                any()) -> {ok, [tuple()]}.
handle_failed_for_session({rsp, _, _} , SessPid, EnvMap, Reason, _Details) ->
    redirect_back({error, Reason}, SessPid, EnvMap);
handle_failed_for_session(_, _, EnvMap, Reason, Details) ->
    ErrMsg = bin_error_msg(Reason, Details),
    redirect_error(ErrMsg, EnvMap).

%% @doc redirect back depending on the result passed in
-spec redirect_back({ok | error, map()}, Session :: pid(), EnvMap::map())
                   -> {ok, [tuple()]}.
redirect_back(Result, SessPid, EnvMap) ->
    {ok, Rsp} = watts_session:get_rsp(SessPid),
    ok = watts:logout(SessPid),
    {ok, [{redirect, get_return_url(Result, Rsp)},
          set_cookie(undefined, EnvMap)]}.

%% @doc get the urls from the rsp and retrun the on that fits the result.
-spec get_return_url({ok | error, map()}, Rsp::watts_rsp:rsp()) -> binary().
get_return_url({ok, _}, Rsp) ->
    {SuccessUrl, _} = watts_rsp:get_return_urls(Rsp),
    SuccessUrl;
get_return_url(_, Rsp) ->
    {_, ErrorUrl} = watts_rsp:get_return_urls(Rsp),
    ErrorUrl.


%% @doc redirect back to the main site with an error session to display
-spec redirect_error(ErrorMsg :: binary(), EnvMap :: map()) -> {ok, [tuple()]}.
redirect_error(ErrorMsg, EnvMap) ->
    {ok, SessPid} = watts:session_with_error(ErrorMsg),
    redirect_set_cookie(SessPid, EnvMap).

%% @doc redirect to the main site and set cookie depending on the session.
-spec redirect_set_cookie(Session :: pid(), EnvMap :: map()) -> {ok, [tuple()]}.
redirect_set_cookie(SessPid, EnvMap) ->
    {ok, [{redirect, ?CONFIG(ep_main)},
          set_cookie(SessPid, EnvMap)]}.

%% @doc set the cookie for a given session or clear it.
-spec set_cookie(Session :: pid() | undefined, EnvMap :: map()) -> tuple().
set_cookie(SessPid, #{req := Req}) when is_pid(SessPid) ->
    {{Ip, _}, Req1} = cowboy_req:peer(Req),
    true = watts_session:is_same_ip(Ip, SessPid),
    {Agent, _} = cowboy_req:header(<<"user-agent">>, Req1),
    true = watts_session:is_user_agent(Agent, SessPid),
    {ok, SessToken} = watts_session:get_sess_token(SessPid),
    {ok, MaxAge} = watts_session:get_max_age(SessPid),
    Opts = watts_http_util:create_cookie_opts(MaxAge),
    CookieName = watts_http_util:cookie_name(),
    {cookie, CookieName, SessToken, Opts};
set_cookie(_, _)  ->
    CookieName = watts_http_util:cookie_name(),
    Opts = watts_http_util:create_cookie_opts(0),
    {cookie, CookieName, <<"deleted">>, Opts}.

%% @doc conver an error and the details into a binary error message
-spec bin_error_msg(Reason :: any(), Details :: any()) -> binary().
bin_error_msg(Reason, Details) ->
    list_to_binary(error_msg(Reason, Details)).

%% @doc conver an error and the details into a string error message
-spec error_msg(Reason :: any(), Details :: any()) -> string().
error_msg(login, _) ->
    "sorry, an internal error occured, please try again";
error_msg(internal,
          {token_invalid, {error, {required_fields_missing, [Field] } } }) ->
    M1 = "The id-token returned from your IdP is missing the ",
    M2 = io_lib:format("required field '~p'. Please contact your IdP.",
                       [Field]),
    io_lib:format("~s~s", [M1, M2]);
error_msg(oidc_provider_error, Error) ->
    M1 = io_lib:format("Your IdP returned you with the error ~p.", [Error]),
    M2 = "Please contact your IdP.",
    io_lib:format("~s ~s", [M1, M2]);
error_msg(internal, {token_invalid, {error, no_id_token}}) ->
    "The token from your IdP is missing an id-token. Please contact your IdP";
error_msg(internal, {token_invalid, _}) ->
    "The token returned from your IdP was invalid, the error has been logged";
error_msg(internal, {bad_user_agent, _}) ->
    "you are not who you was before, incident has been logged";
error_msg(internal, {bad_peer_ip, _}) ->
    "stealing cookies is a crime, you have been logged";
error_msg(internal, {bad_cookie, _}) ->
    "sorry, been unable to recognize you, please try again.";
error_msg(session_not_found, _) ->
    "sorry, your session expired during the login process, please try again.";
error_msg(_, _) ->
    "sorry, something went wrong, please try again".


%% @doc get the session and its type from a cowboy request
-spec get_session_type(map())
                      -> {atom() | tuple(), undefined | pid()}.
get_session_type(#{req := Req}) ->
    {Cookies, _} = cowboy_req:cookies(Req),
    Cookie = lists:keyfind(watts_http_util:cookie_name(), 1, Cookies),
    get_cookie_session_type(Cookie).

%% @doc get the session and its type from cookie data
-spec get_cookie_session_type(any()) -> {atom() | tuple(), undefined | pid() }.
get_cookie_session_type({_, Cookie}) when is_binary(Cookie) ->
    Result =  watts_session_mgr:get_session(Cookie),
    return_session_type(Result);
get_cookie_session_type(Other) ->
    return_session_type(Other).

%% @doc return the session and its type might also be undefined
-spec return_session_type(any()) -> {atom() | tuple() , undefined | pid() }.
return_session_type({ok, Pid}) when is_pid(Pid) ->
    {ok, Type} = watts_session:get_type(Pid),
    {Type, Pid};
return_session_type(_) ->
    {none, undefined}.

%% @doc unused function to be compliant with behaviour
-spec login_succeeded(any()) -> {ok, [tuple()]}.
login_succeeded(_TokenMap) ->
    {ok, [{redirect, watts_http_util:whole_url(<<"/">>)}]}.

%% @doc unused function to be compliant with behaviour
-spec login_failed(any(), any()) -> {ok, [tuple()]}.
login_failed(_Reason, _Details) ->
    {ok, [{redirect, watts_http_util:whole_url(<<"/">>)}]}.
