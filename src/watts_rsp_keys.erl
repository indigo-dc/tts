%% @doc this module implements a gen_server that keeps the public keys of
%% an RSP up to date.
%% So the signed jwts can always be validated.
-module(watts_rsp_keys).
-behaviour(gen_server).

-include("watts.hrl").
-export([
         new/1,
         start_link/1,
         stop/1,
         get_keys/1,
         get_last_error/1,
         reread/1]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).
-export_type([config/0]).

-record(state, {
          id = undefined :: undefined | binary(),
          keys = [],
          key_location = undefined :: undefined | binary(),
          location_type = none :: none | file | url,
          http_req_id = undefined,
          http_result = undefined,
          reading_keys = false,
          last_error = undefined,
          next_try = undefined
         }).


-define(IDLE_TIMEOUT, 180000). %% every 3 minutes

-type config() :: #{ id => binary(),
                     key_location => binary(),
                     _ => _
                   }.
-type state() :: #state{}.
-type keys() :: [key()].
-type key() :: map().

%% @doc start a new rsp gen_server process
-spec new(config()) -> {ok, pid()}.
new(Map) when is_map(Map) ->
    watts_rsp_keys_sup:new_rsp_keys(Map).

%% @doc start the gen_server linked to the supervisor
-spec start_link(config()) -> {ok, pid()}.
start_link(Map) when is_map(Map) ->
    gen_server:start_link(?MODULE, Map, []).

%% @doc stop the process
-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

%% @doc triger a reading of the keys
-spec reread(pid()) -> ok.
reread(Pid) ->
    gen_server:cast(Pid, read_keys).

%% @doc return the chached keys
-spec get_keys(pid() | undefined) -> {ok, keys()}.
get_keys(undefined) ->
    {ok, []};
get_keys(Pid) ->
    gen_server:call(Pid, get_keys).

%% @doc return the last error
-spec get_last_error(pid()) ->  {ok, any()}.
get_last_error(Pid) ->
    gen_server:call(Pid, get_last_error).

%% @doc initilize the process
-spec init(config()) -> {ok, state()}.
init(#{id := Id, key_location := Location0}) ->
    {Location, Type} = get_location_type(Location0),
    maybe_trigger_key_read(true),
    {ok,  #state{id = Id, key_location = Location, location_type = Type}}.

%% @doc handle the get error/keys calls
-spec handle_call(any(), any(), state())
                 -> {reply, any(), state(), non_neg_integer()}.
handle_call(get_keys, _From, #state{keys = Keys} = State) ->
    {reply, {ok, Keys}, State, ?IDLE_TIMEOUT};
handle_call(get_last_error, _From, #state{last_error = Error} = State) ->
    {reply, {ok, Error}, State, ?IDLE_TIMEOUT};
handle_call(_Request, _From, State) ->
    reread_keys_if_needed(State),
    {reply, ignored, State, ?IDLE_TIMEOUT}.

%% @doc handle the request to reread keys or stop
-spec handle_cast(any(), state()) -> {noreply, state(), non_neg_integer()} |
                                     {stop, normal, state()}.
handle_cast(read_keys, #state{reading_keys = Reading,
                              http_req_id = Id} = State) ->
    maybe_cancel_request(Reading, Id),
    NewState = handle_key_result(get_rsp_keys(State)),
    {noreply, NewState, ?IDLE_TIMEOUT};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, State) ->
    reread_keys_if_needed(State),
    {noreply, State, ?IDLE_TIMEOUT}.

%% @doc handle the async incoming http responses when fetching keys
-spec handle_info(any(), state()) -> {noreply, state(), non_neg_integer()}.
handle_info({http, {ReqId, Result}}, #state{http_req_id = ReqId} = State) ->
    NewState = handle_http_result(State#state{http_result = Result}),
    {noreply, NewState, ?IDLE_TIMEOUT};
handle_info(_Request, State) ->
    reread_keys_if_needed(State),
    {noreply, State, ?IDLE_TIMEOUT}.

%% @doc handle the http response.
-spec handle_http_result(state()) -> state().
handle_http_result(#state{http_result={error, Reason}} = State) ->
    handle_http_client_crash(Reason, State);
handle_http_result(#state{http_result={{_Proto, Status, _StatusName}, Header,
                                       InBody}
                         } = State) ->
    GoodStatus = (Status >= 200) and (Status < 300),
    {ok, Body} = oidcc_http_util:uncompress_body_if_needed(InBody, Header),
    handle_http_result(GoodStatus, Status, Body, State).

%% @doc handle crashing of the http client
-spec handle_http_client_crash(any(), state()) -> state().
handle_http_client_crash(Reason, State) ->
    State#state{http_req_id=undefined, http_result={},
                last_error = {client_crash, Reason},
                next_try = deadline_in(300)}.


%% @doc handle a  http result and extract rsp keys
-spec handle_http_result(boolean(), integer(), binary(), state()) -> state().
handle_http_result(true, Status, Body, State) ->
    handle_key_result({extract_rsp_keys({ok, {Status, Body}}), State});
handle_http_result(false, Status, Body, #state{id = Id} = State) ->
    lager:warning("RSP ~p: error fetching keys: http status ~p", [Id, Status]),
    State#state{last_error = {http_result, Status, Body},
                next_try = deadline_in(300)}.


%% @doc get the location type, either file or url of the keys
-spec get_location_type(binary()) -> {binary(), file | url} | {none, unknown}.
get_location_type(<< File:7/binary, Path/binary >>)
  when File == <<"file://">> ->
    {Path, file};
get_location_type(<< Https:8/binary, _Rest/binary >> = Url)
  when Https == <<"https://">> ->
    {Url, url};
get_location_type(<< Http:7/binary, _Rest/binary >> = Url)
  when Http == <<"http://">> ->
    lager:warning("fetching rsp keys over a non secure connection: ~p!", [Url]),
    {Url, url};
get_location_type(_) ->
    {none, unknown}.


%% @doc get the keys by reading the file or triggering the http request
-spec get_rsp_keys(state()) -> {{ok, keys()} | {error, any()}, state()}.
get_rsp_keys(#state{key_location = Path, location_type = file} = State) ->
    Result = file:read_file(binary_to_list(Path)),
    {extract_rsp_keys(Result), State};
get_rsp_keys(#state{key_location = Url, location_type = url} = State) ->
    NewState = fetch_rsp_keys(Url, State),
    {{error, none}, NewState}.


%% @doc trigger the async http request
-spec fetch_rsp_keys(binary(), state()) -> state().
fetch_rsp_keys(Url, State) ->
    case oidcc_http_util:async_http(get, Url, []) of
        {ok, RequestId} ->
            State#state{http_req_id = RequestId, reading_keys=true};
        Error ->
            State#state{last_error = {http_request, Error}}
    end.

%% @doc extract the keys from the data (either file or http response)
-spec extract_rsp_keys({ok | integer(), string() | binary()} | {error, any()})
                      -> {ok, keys()} | {error, any()}.
extract_rsp_keys({ok, Data}) when is_binary(Data) ->
    handle_decode_result(safe_decode(Data, bad_data));
extract_rsp_keys({ok, Data}) when is_list(Data) ->
    extract_rsp_keys({ok, list_to_binary(Data)});
extract_rsp_keys({ok, {Code, Body}}) when Code >= 200, Code < 300 ->
    extract_rsp_keys({ok, Body});
extract_rsp_keys({error, Reason}) ->
    {error, list_to_binary(io_lib:format("error reading keys: ~p", [Reason]))}.

%% @doc handle the decode result
-spec handle_decode_result(map() | any()) -> {ok, keys()} | {error, bad_data}.
handle_decode_result(#{keys := Keys}) ->
    {ok, Keys};
handle_decode_result(_) ->
    {error, bad_data}.

%% @doc handle the decoding result, either store the keys in stat or skip
-spec handle_key_result({{ok, keys()} | {error, any()}, state()}) -> state().
handle_key_result({{ok, Keys}, State}) ->
    State#state{keys = Keys, reading_keys = false, next_try=deadline_in(900),
                last_error = undefined};
handle_key_result({{error, none}, State}) ->
    State;
handle_key_result({Error, #state{id = Id, location_type = url} = State}) ->
    lager:warning("RSP ~p: error fetching keys: ~p", [Id, Error]),
    State#state{last_error = {key_page, Error}, reading_keys = false,
                next_try = deadline_in(90)};
handle_key_result({Error, #state{id = Id, location_type = file} = State}) ->
    lager:warning("RSP ~p: error reading key-file: ~p", [Id, Error]),
    State#state{reading_keys = false, last_error = {key_file, Error},
                next_try = deadline_in(300)}.


%% @doc safely decode any data by returning on success or default on failure
-spec safe_decode(binary(), any()) -> any().
safe_decode(Data, Default) when is_binary(Data) ->
    Res = jsone:try_decode(Data, [{object_format, map}, {keys, attempt_atom}]),
    result_or_default(Res, Default).

%% @doc return either value if present, if not default
-spec result_or_default({ok, any()} | any(), any()) -> any().
result_or_default({ok, Json, _}, _) ->
    Json;
result_or_default(_, Default) ->
    Default.


%% @doc maybe cancel a current running http request
-spec maybe_cancel_request(boolean(), any()) -> ok.
maybe_cancel_request(false, _) ->
    ok;
maybe_cancel_request(true, Id) ->
    httpc:cancel_request(Id).

%% @doc do nothing
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_, State, _) ->
    {ok, State}.

%% @doc do nothing
-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc calculate the next deadline
-spec deadline_in(non_neg_integer()) -> non_neg_integer().
deadline_in(Seconds) ->
    erlang:system_time(seconds) + Seconds.

%% @doc rereadkeys if deadline reached
-spec reread_keys_if_needed(state()) -> ok.
reread_keys_if_needed(#state{reading_keys = true}) ->
    ok;
reread_keys_if_needed(#state{next_try=Seconds}) ->
    maybe_trigger_key_read(Seconds =< deadline_in(0)).

%% @doc maybe trigger reading the keys again
-spec maybe_trigger_key_read(boolean()) -> ok.
maybe_trigger_key_read(true) ->
    gen_server:cast(self(), read_keys);
maybe_trigger_key_read(_) ->
    ok.
