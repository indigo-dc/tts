%% @doc a dedicated module to creating the error messages and mails for plugins
-module (watts_plugin_error).
%%
%% Copyright 2017 SCC/KIT
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0 (see also the LICENSE file)
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-author("Bas Wegh, Bas.Wegh<at>kit.edu").
-include("watts.hrl").

-export([maybe_send_mail_return_error/4]).


%% @doc create the error and if configured send out the mail
-spec maybe_send_mail_return_error(Result, MapOrReason, Output, Info)
                                  -> {error, #{ user_msg => string(),
                                                log_msg => string()}}
    when
      Result :: ok | error | oidc_login | plugin_error | undefined | any(),
      MapOrReason :: map() | any(),
      Output :: watts_plugin_runner:output(),
      Info :: map().
maybe_send_mail_return_error(Result, Map, Output, Info) ->
    ResultMap = create_mail_log(Result, Map, Output, Info),
    ok = maybe_send_mail(?CONFIG(send_mail), maps:get(mail, ResultMap, #{}),
                         Info),
    {error, maps:with([user_msg, log_msg], ResultMap)}.


%% @doc create the log result of the plugin run, only error
-spec create_mail_log( Result, MapOrReason, Output, Info)
                      -> #{user_msg => string(), log_msg => string()}
    when
      Result :: ok | error | oidc_login | plugin_error | undefined | any(),
      MapOrReason :: map() | any(),
      Output :: watts_plugin_runner:output(),
      Info :: map().
create_mail_log(Result, Map, Output, #{action := request})
  when Result == ok; Result == oidc_login ->
    %% a bad OpenID Connect login request
    LogMsg = io_lib:format("bad response to a request: ~p ~s",
                           [Map, log_output(Output)]),
    UMsg = "the plugin returned a bad result, please contact the administrator",
    #{user_msg => UMsg, log_msg => LogMsg};
create_mail_log(error, Map, Output, #{ action := Action})
  when Action == request ; Action == revoke->
    %% a bad error response
    LogMsg = io_lib:format("bad error response: ~p ~s", [Map,
                                                         log_output(Output)]),
    UMsg = "the plugin returned a bad result, please contact the administrator",
    #{user_msg => UMsg, log_msg => LogMsg};
create_mail_log(ok, Result, Output, #{ action := parameter } ) ->
    %% invalid parameter response
    NeededKeys = [conf_params, request_params, version, result],
    Keys = maps:keys(Result),
    MissingKeys = lists:subtract(NeededKeys, Keys),
    LogMsg =
        io_lib:format("bad parameter response: ~p missing keys ~p (type?) ~s",
                      [Result, MissingKeys, log_output(Output)]),
    UMsg = "the plugin returned a bad result, please contact the administrator",
    #{log_msg => LogMsg, user_msg => UMsg};
create_mail_log(error, Result, Output, #{ action := parameter } ) ->
    %% invalid parameter response
    LogMsg = io_lib:format("bad parameter response: ~p ~s",
                           [Result, log_output(Output)]),
    UMsg = "the plugin returned a bad result, please contact the administrator",
    #{log_msg => LogMsg, user_msg => UMsg};
create_mail_log(plugin_error, Reason, Output, _Info) ->
    LogMsg = io_lib:format("plugin error: ~p ~s", [Reason, log_output(Output)]),
    UMsg = "the plugin had an error, please contact the administrator",
    #{user_msg => UMsg, log_msg=>LogMsg};
create_mail_log(undefined, _Map, _Output, #{service_id := ServiceId}) ->
    LogMsg = io_lib:format("plugin missing 'result': service ~p", [ServiceId]),
    UMsg = "the plugin had an error, please contact the administrator",
    #{user_msg => UMsg, log_msg => LogMsg};
create_mail_log(Result, MaybeMap, Output, #{action := Action}) ->
    LogMsg = io_lib:format("plugin unknown error at ~p: ~p - ~p ~s",
                           [Action, Result, MaybeMap, log_output(Output)]),
    UMsg = "the plugin had an error, please contact the administrator",
    #{user_msg => UMsg, log_msg => LogMsg}.

%% @doc create a loggable string from the output map
-spec log_output(watts_plugin_runner:output()) -> string().
log_output(#{cmd := Cmd, env := Env, std_out := StdOut, std_err := StdErr}) ->
    String = "~n  cmd: ~p~n  env: ~p~n  out: ~p~n  err: ~p~n",
    io_lib:format(String, [Cmd, Env, StdOut, StdErr]).



%% @doc send out the error mail
-spec maybe_send_mail(boolean(), map(), map()) -> ok.
maybe_send_mail(true, #{subject := Subject, body := Body},
                #{service_id := ServiceId}) ->
    {ok, #{mail_on_error_to := MailTo,
           devel_email := DeveloperMail
          }} = watts_service:get_info(ServiceId),
    Receipients = receipients(MailTo, DeveloperMail, ?CONFIG(admin_mail)),
    watts_mail:send(Subject, Body, Receipients);
maybe_send_mail(_, _, _) ->
    ok.

receipients(admin, _, Mail) ->
    [Mail];
receipients(developer, Mail, _) ->
    [Mail];
receipients(admin_devel, DMail, AMail) ->
    [DMail, AMail];
receipients(noone, _, _) ->
    [].
