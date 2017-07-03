-module(watts_rsp_keys).
-behaviour(gen_server).

-include("watts.hrl").
-export([
         new/1,
         start_link/1,
         get_keys/1,
         reread/1]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state, {
          id = undefined,
          keys = [],
          key_location = undefined,
          location_type = undefined,
          http_req_id = undefined,
          http_result = undefined,
          reading_keys = false,
          last_error = undefined,
          next_try = undefined
         }).


-define(IDLE_TIMEOUT, 180000). %% every 3 minutes

new(Map) when is_map(Map) ->
    watts_rsp_keys_sup:new_rsp_keys(Map).

start_link(Map) when is_map(Map) ->
    gen_server:start_link(?MODULE, Map, []).

reread(Pid) ->
    gen_server:cast(Pid, read_keys).

get_keys(Pid) ->
    gen_server:call(Pid, get_keys).

init(#{id := Id, key_location := Location0}) ->
    {Location, Type} = get_location_type(Location0),
    maybe_trigger_key_read(true),
    {ok,  #state{id = Id, key_location = Location, location_type = Type}}.

handle_call(get_keys, _From, #state{keys = Keys} = State) ->
    {reply, {ok, Keys}, State, ?IDLE_TIMEOUT};
handle_call(_Request, _From, State) ->
    reread_keys_if_needed(State),
    {reply, ignored, State, ?IDLE_TIMEOUT}.

handle_cast(read_keys, #state{reading_keys = false} = State) ->
    NewState = handle_key_result(get_rsp_keys(State)),
    {noreply, NewState#state{reading_keys = true}, ?IDLE_TIMEOUT};
handle_cast(_Request, State) ->
    reread_keys_if_needed(State),
    {noreply, State, ?IDLE_TIMEOUT}.

handle_info({http, {ReqId, Result}}, #state{http_req_id = ReqId} = State) ->
    NewState = handle_http_result(State#state{http_result = Result}),
    {noreply, NewState, ?IDLE_TIMEOUT};
handle_info(_Request, State) ->
    reread_keys_if_needed(State),
    {noreply, State, ?IDLE_TIMEOUT}.


handle_http_result(true, Status,  _Header, Body, State) ->
    handle_key_result({extract_rsp_keys({ok, {Status, Body}}), State});
handle_http_result(false, Status, _Header, Body, #state{id = Id} = State) ->
    lager:warning("RSP ~p: error fetching keys: http status ~p", [Id, Status]),
    State#state{last_error = {http_result, Status, Body},
                next_try = deadline_in(300)}.


handle_http_result(#state{http_result={error, Reason}} = State) ->
    handle_http_client_crash(Reason, State);
handle_http_result(#state{http_result={{_Proto, Status, _StatusName}, Header,
                                       InBody}
                         } = State) ->
    GoodStatus = (Status >= 200) and (Status < 300),
    {ok, Body} = oidcc_http_util:uncompress_body_if_needed(InBody, Header),
handle_http_result(GoodStatus, Status, Header, Body, State).

handle_http_client_crash(Reason, State) ->
    State#state{http_req_id=undefined, http_result={},
                last_error = {client_crash, Reason},
                next_try = deadline_in(300)}.


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


get_rsp_keys(#state{key_location = Path, location_type = file} = State) ->
    Result = file:read_file(binary_to_list(Path)),
    {extract_rsp_keys(Result), State};
get_rsp_keys(#state{key_location = Url, location_type = url} = State) ->
    NewState = fetch_rsp_keys(Url, State),
    {none, NewState}.

fetch_rsp_keys(Url, State) ->
    case oidcc_http_util:async_http(get, Url, []) of
        {ok, RequestId} ->
            State#state{http_req_id = RequestId, reading_keys=true};
        Error ->
            State#state{last_error = {http_request, Error}}
    end.

extract_rsp_keys({ok, Data}) when is_binary(Data) ->
    handle_decode_result(safe_decode(Data, bad_data));
extract_rsp_keys({ok, Data}) when is_list(Data) ->
    extract_rsp_keys({ok, list_to_binary(Data)});
extract_rsp_keys({ok, {Code, Body}}) when Code >= 200, Code < 300 ->
    extract_rsp_keys({ok, Body});
extract_rsp_keys({error, Reason}) ->
    {error, list_to_binary(io_lib:format("error reading keys: ~p", [Reason]))}.

handle_decode_result(#{keys := Keys}) ->
    {ok, Keys};
handle_decode_result(_) ->
    {error, bad_data}.



handle_key_result({{ok, Keys}, State}) ->
    State#state{keys = Keys, reading_keys = false, next_try=deadline_in(900),
                last_error = undefined};
handle_key_result({none, State}) ->
    State;
handle_key_result({Error, #state{id = Id, location_type = url} = State}) ->
    lager:warning("RSP ~p: error fetching keys: ~p", [Id, Error]),
    State#state{last_error = {key_page, Error}, reading_keys = false,
                next_try = deadline_in(90)};
handle_key_result({Error, #state{id = Id, location_type = file} = State}) ->
    lager:warning("RSP ~p: error reading key-file: ~p", [Id, Error]),
    State#state{reading_keys = false, last_error = {key_file, Error},
                next_try = deadline_in(300)}.

safe_decode(Data, Default) when is_binary(Data) ->
    Res = jsone:try_decode(Data, [{object_format, map}, {keys, attempt_atom}]),
    safe_decode(Res, Default);
safe_decode({ok, Json, _}, _) ->
    Json;
safe_decode(_, Default) ->
    Default.


code_change(_, _, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

deadline_in(Seconds) ->
    erlang:system_time(seconds) + Seconds.

reread_keys_if_needed(#state{reading_keys = true}) ->
    ok;
reread_keys_if_needed(#state{next_try=Seconds}) ->
    maybe_trigger_key_read(Seconds =< deadline_in(0)).

maybe_trigger_key_read(true) ->
    gen_server:cast(self(), read_keys);
maybe_trigger_key_read(_) ->
    ok.
