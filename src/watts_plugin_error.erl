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

-type error_type() :: bad_result | bad_error | bad_parameter | error_parameter |
                      plugin_error | missing_result | unknown.

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
    ErrorType = to_error_type(Result, Info),
    LogMsg = log_message(ErrorType, Map, Output),
    SendMailOnError = ?CONFIG(mail_on_plugin_error, false),
    Mail = create_mail(ErrorType, Map, Output, Info),
    DoSend = SendMailOnError and (maps:size(Mail) == 2),
    MailSent = case maybe_send_mail(DoSend, Mail, Info) of
                   ok ->
                       true;
                   _ ->
                       false
               end,
    UserMsg = user_message(ErrorType, MailSent ),
    {error, #{log_msg => LogMsg, user_msg => UserMsg}}.

%% @doc convert the error condition to a single atom
-spec to_error_type(Result, Info) -> error_type()
   when
      Result :: ok | error | oidc_login | plugin_error | undefined | any(),
      Info :: map().
to_error_type(Result, #{action := request})
  when Result == ok; Result == oidc_login ->
    bad_result;
to_error_type(error, #{action := Action})
  when Action == request ; Action == revoke->
    bad_error;
to_error_type(ok,  #{ action := parameter } ) ->
    bad_parameter;
to_error_type(error, #{ action := parameter } ) ->
    error_parameter;
to_error_type(plugin_error, _Info) ->
    plugin_error;
to_error_type(undefined, _Info) ->
    missing_result;
to_error_type(_Result, _Info) ->
    unknown.

%% @doc create the log result of the plugin
%% @todo alter if mail has been sent
-spec log_message( ErrorType, MapOrReason, Output) -> string()
    when
      ErrorType :: error_type(),
      MapOrReason :: map() | any(),
      Output :: watts_plugin_runner:output().
log_message(bad_result, Map, Output) ->
    %% a bad result
    io_lib:format("bad response to a request: ~p~s", [Map, log_output(Output)]);
log_message(bad_error, Map, Output) ->
    %% a bad error response
    io_lib:format("bad error response: ~p ~s", [Map, log_output(Output)]);
log_message(bad_parameter, Result, Output ) ->
    %% invalid parameter response
    NeededKeys = [conf_params, request_params, version, result],
    Keys = maps:keys(Result),
    MissingKeys = lists:subtract(NeededKeys, Keys),
    io_lib:format("bad parameter response: ~p missing keys ~p (type?) ~s",
                  [Result, MissingKeys, log_output(Output)]);
log_message(error_parameter, Result, Output) ->
    %% invalid parameter response
    io_lib:format("bad parameter response: ~p~s", [Result, log_output(Output)]);
log_message(plugin_error, Reason, Output) ->
    %% the plugin did not return valid json
    io_lib:format("plugin error: ~p~s", [Reason, log_output(Output)]);
log_message(missing_result, Map, Output) ->
    io_lib:format("plugin response is missing 'result' ~p ~s",
                  [Map, log_output(Output)]);
log_message(unknown, MaybeMap, Output) ->
    io_lib:format("plugin unknown error ~p ~s", [MaybeMap, log_output(Output)]).


%% @doc create the user message for the error, state if email has been sent
-spec user_message(error_type(), boolean()) -> string().
user_message(ErrorType, false)
  when ErrorType == plugin_error; ErrorType == missing_result;
       ErrorType == unknown ->
    "the plugin had an error, please contact the administrator";
user_message(ErrorType, true)
  when ErrorType == plugin_error; ErrorType == missing_result;
       ErrorType == unknown ->
    "the plugin had an error, an email about the issue has been sent";
user_message(_, false) ->
    "the plugin returned a bad result, please contact the administrator";
user_message(_, true) ->
    "the plugin returned a bad result, an email about the issue has been sent".


%% @doc create email subject and body for the error
-spec create_mail(ErrorType, MapOrReason, Output, Info)
                 -> #{subject => string(), body => string()}
   when
      ErrorType :: error_type(),
      MapOrReason :: map() | any(),
      Output :: watts_plugin_runner:output(),
      Info :: map().
create_mail(ErrorType, Map, Output, Info) ->
    Subject = error_type_to_subject(ErrorType),
    Body = create_body(ErrorType, Map, Output, Info),
    #{subject => Subject, body => Body}.

%% @doc convert the error type to the email subject
-spec error_type_to_subject(error_type()) -> string().
error_type_to_subject(bad_result) ->
    "plugin returned a bad result";
error_type_to_subject(bad_error) ->
    "plugin returned a bad error";
error_type_to_subject(bad_parameter) ->
    "plugin returned a bad parameter";
error_type_to_subject(error_parameter) ->
    "plugin returned an error to parameter";
error_type_to_subject(plugin_error) ->
    "plugin returned invalid json";
error_type_to_subject(missing_result) ->
    "plugin returned json without 'result'";
error_type_to_subject(unknown) ->
    "plugin had an unspecified error".


%% @doc create the email body and provide as much information as possible
-spec create_body(ErrorType, MapOrReason, Output, Info) -> string()
   when
      ErrorType :: error_type(),
      MapOrReason :: map() | any(),
      Output :: watts_plugin_runner:output(),
      Info :: map().
create_body(ErrorType, Map, Output, Info) ->
    Action = maps:get(action, Info),
    ServiceId = maps:get(service_id, Info),
    {ok, #{cmd := Cmd,
           plugin_version := Version
          }} = watts_service:get_info(ServiceId),
    Desc = error_type_to_description(ErrorType),
    io_lib:format(
      "Hello Developer/Admin,~n"
      "I detected an issue with a plugin and wanted to inform you about it.~n~n"
      "~s~n~n"
      "Some more details about the issue:~n"
      "The affected service id was: ~p~n"
      "The used plugin was: ~p version ~p~n"
      "The action was: ~p~n~n"
      "The parsed response was: ~n ~p~n~n"
      "The raw output was: ~s~n~n"
      "I wish you a great day,~n WaTTS",
      [Desc, ServiceId, Cmd, Version, Action, Map, log_output(Output) ]).


%% @doc convert the type to reasonable helpful description of the error
-spec error_type_to_description(error_type()) -> string().
error_type_to_description(ErrorType)
  when ErrorType == bad_result; ErrorType == bad_error->
    "The plugin returned valid json that does not contain "
        "all the needed keys.~n"
        "For a result=ok it also needs: credential and state~n"
        "For a result=oidc_login it also needs: provider and msg~n"
        "For a result=error it also needs: user_msg and (optional) log_msg~n";
error_type_to_description(ErrorType)
  when ErrorType == bad_parameter; ErrorType == error_parameter->
    "The plugin returned valid json that does not contain "
        "all the needed keys or wrong values.~n"
        "The expected result for a action=parameter request is:~n"
        " - result: ok~n"
        " - version: the version of the plugin~n"
        " - conf_params: a list of configuration params (see documentation)~n"
        " - request_params: a list of parameter sets (see documentation)~n"
        " - developer_email: (optional) the email of the developer for issues"
        "like this~n" ;
error_type_to_description(ErrorType)
  when ErrorType == plugin_error->
    "The plugin did not return valid json, so I was not able to "
        "give more information about the reason.~n";
error_type_to_description(ErrorType)
  when ErrorType == missing_result->
    "The plugin returned valid json that does not contain "
        "the result key so I was not able to process it further.~n";
error_type_to_description(ErrorType)
  when ErrorType == missing_result->
    "An undetected and unspecified error occured. This should not happen "
        "and I am very sorry about it. Could you please inform my creators "
        "about this issue? The best would be if you create an issue on github, "
        "thanks in advance for that.".


%% @doc create a loggable string from the output map
-spec log_output(watts_plugin_runner:output()) -> string().
log_output(#{cmd := Cmd, env := Env, std_out := StdOut, std_err := StdErr}) ->
    String = "~n  cmd: ~p~n  env: ~p~n  out: ~p~n  err: ~p~n",
    io_lib:format(String, [Cmd, Env, StdOut, StdErr]).


%% @doc send out the error mail
-spec maybe_send_mail(boolean(), map(), map()) -> atom().
maybe_send_mail(true, #{subject := Subject, body := Body}, Info) ->
    ServiceId = maps:get(service_id, Info, undefined),
    {ok, #{mail_on_error_to := MailTo,
           devel_email := DeveloperMail
          }} = watts_service:get_info(ServiceId),
    Receipients = receipients(MailTo, DeveloperMail, ?CONFIG(admin_mail)),
    watts_mail:send(Subject, Body, Receipients);
maybe_send_mail(_, _, _) ->
    not_sent.


%% @doc create the list of receipients depending on the configuration
-spec receipients(atom(), string(), string()) -> [string()].
receipients(admin, _, Mail) ->
    [Mail];
receipients(developer, Mail, _) ->
    [Mail];
receipients(admin_devel, DMail, AMail) ->
    [DMail, AMail];
receipients(noone, _, _) ->
    [].
