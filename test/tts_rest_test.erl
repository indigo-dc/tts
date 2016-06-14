-module(tts_rest_test).
-include_lib("eunit/include/eunit.hrl").


%% -export([dispatch_mapping/1]).
%%
%% -export([init/3]).
%% -export([rest_init/2]).
%% -export([allowed_methods/2]).
%% -export([allow_missing_post/2]).
%% -export([content_types_provided/2]).
%% -export([content_types_accepted/2]).
%% -export([is_authorized/2]).
%% -export([malformed_request/2]).
%% -export([resource_exists/2]).
%% -export([get_json/2]).
%% -export([post_json/2]).
%% -export([delete_resource/2]).

dispatch_mapping_test() ->
    BasePath1 = <<"/test">>,
    BasePath2 = <<"/test/">>,

    ExpMapping = <<"/test/[:version]/:type/[:id]">>,

    Mapping1 = tts_rest:dispatch_mapping(BasePath1),
    Mapping2 = tts_rest:dispatch_mapping(BasePath2),
    ?assertEqual(ExpMapping, Mapping1),
    ?assertEqual(Mapping1, Mapping2),
    ok.


init_test() ->
    ?assertEqual({upgrade, protocol, cowboy_rest}, tts_rest:init(a,b,c)),
    ok.

%% state from tts_rest
-record(state, {
          method = undefined,
          version = undefined,
          type = undefined,
          id = undefined,
          token = undefined,
          issuer = undefined,
          object = undefined,
          provider = undefined,
          json = undefined,
          user_info = undefined
         }).

rest_init_test() ->
    Req = req,
    {ok, Req, #state{}} = tts_rest:rest_init(Req, doesnt_matter),
    ok.

allowed_methods_test() ->
    State = #state{},
    Req = req,
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State} =
    tts_rest:allowed_methods(Req, State),
    ok.

allow_missing_post_test() ->
    State = #state{},
    Req = req,
    {false, Req, State} = tts_rest:allow_missing_post(Req, State).

content_types_provided_test() ->
    State = #state{},
    Req = req,
    {[ContentType], Req, State} = tts_rest:content_types_provided(Req, State),
    {{<<"application">>, <<"json">>, '*'}, get_json} = ContentType,
    ok.

content_types_accepted_test() ->
    State = #state{},
    Req = req,
    {[ContentType], Req, State} = tts_rest:content_types_accepted(Req, State),
    {{<<"application">>, <<"json">>, '*'}, post_json} = ContentType,
    ok.

is_authorized_test() ->
    ok.

malformed_request_test() ->
    ok.

resource_exists_test() ->
    ok.

get_json_test() ->
    ok.

post_json_test() ->
    ok.

delete_resource_test() ->
    ok.
