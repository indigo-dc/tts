%% @doc this module is only handling incomming http requests
%% and sends them a 303 to the httpS location.
-module(watts_http_redirect).
-behaviour(cowboy_http_handler).
-export([
         init/3,
         terminate/3,
         handle/2]).

%% @doc this just returns the passed request
-spec init(any(), cowboy_req:req(), any()) -> {ok, cowboy_req:req(), []}.
init(_Type, Req, _Opts) ->
    {ok, Req, []}.

%% @doc on termination nothing needs to be done
-spec terminate(any(), any(), any()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%% @doc handle all request by taking the relative path and
%% returning a redirection to the SSL path
-spec handle(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
handle(Req, State) ->
    {Path, Req2} = cowboy_req:path(Req),
    Url = watts_http_util:whole_url(binary_to_list(Path)),
    Req3 = watts_http_util:redirect_to(Url, Req2),
    {ok, Req3, State}.
