-module(tts).


-export([
         start_full_debug/0,
         start_debug/1,
         start_debug/2,
         stop_debug/0,
         set_debug_mode/1
        ]).

-export_type([
              oidc_id/0,
              user_info/0,
              cred/0
             ]).

-type oidc_id() :: { Issuer:: binary(), Subject::binary() }.

-type user_info() :: #{ uid => binary(),
                        uidNumber => integer(),
                        gidNumber => integer(),
                        homeDirectory => binary(),
                        userIds => [oidc_id()]
                      }.

-type cred() :: #{ cred_id => binary(), ctime => integer(),
                   cred_state => binary(), service_id => binary(),
                   interface => binary(), user_id => binary()
                 }.


start_full_debug() ->
    %debug these modules
    ListOfModules = [ "tts_http",
                      "tts_rest",
                      "tts_rest_cred",
                      "tts_user_cache",
                      "tts_session",
                      "tts_session_mgr",
                      "tts_idh",
                      "tts_idh_worker",
                      "tts_service",
                      "tts_credential",
                      "tts_cred_worker"
                    ],
    start_debug(ListOfModules).

set_debug_mode(OnOff) ->
    application:set_env(tts, debug_mode, OnOff).

start_debug(ListOfModules) ->
    %debug for an hour or 10000 messages
    Options = [{time, 3600000}, {msgs, 10000}],
    start_debug(ListOfModules, Options).

start_debug(ListOfModules, Options) ->
    redbug:start(ListOfModules, Options).


stop_debug() ->
    redbug:stop().
