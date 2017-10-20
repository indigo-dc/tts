%% @doc The purpose of this module is to ensure that the JWT keys are rotated and availeable.
%%
%% The module does
%% <ul>
%% <li> trigger a check every minute through the idle timeout {@link maybe_update/1 } </li>
%% <li> Then checks if old keys can be deleted </li>
%% <li> Also checks if new keys need to be created. This is configured
%% with the variable 'jwt_key_rotation_interval' </li>
%% </ul>
-module(watts_jwt_keys).

-behaviour(gen_server).

-include_lib("public_key/include/public_key.hrl").
-include("watts.hrl").
-export([
         start_link/0,
         initial_read/0,
         stop/0]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state, {
          next_key_rotation = 0 :: integer(),
          next_key_delete = undefined :: undefined | pos_integer(),
          key_file = <<>> :: binary(),
          key_bits = <<>> :: binary(),
          info_file = <<>> :: binary()
         }).
-type state() :: #state{}.
-type key() :: #{kty => binary(), n => binary(), e => binary(), d => binary()}.

-ifdef(TEST).
-define(IDLE_TIMEOUT, 100). %% 100ms
-else.
-define(IDLE_TIMEOUT, 60000). %% every minute
-endif.
%% @doc start the gen_server linked to the current process
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, noparams, []).

%% @doc stop the gen_server
-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

%% @doc trigger the initial read or generation of the jwt signing key
-spec initial_read() -> ok | error.
initial_read() ->
    gen_server:call(?MODULE, read_key).

%% @doc initialize the gen_server.
%% This waits if it has not yet started before, if it has started before
%% it triggers a direct rotation as it has crashed and lost track of time.
-spec init(noparams) -> {ok, state()} | {ok, state(), pos_integer()}.
init(noparams) ->
    KeyDir = ?CONFIG(secret_dir),
    KeyFile = filename:join(KeyDir, <<"jwt.key">>),
    InfoFile = filename:join(KeyDir, <<"jwt-key.info">>),
    Bits = ?CONFIG(jwt_key_bits, <<"2048">>),
    BaseState = #state{key_file = KeyFile,
                       info_file = InfoFile,
                       key_bits = Bits},
    maybe_trigger_direct_rotation(BaseState).

%% @doc this triggers a direct rotation after a crash.
%% as through a crash the tack of time is lost, we just assume it is now.
%% so setting the key rotation time to now and setting timeout to 1 ms.
-spec maybe_trigger_direct_rotation(state())
                                   -> {ok, state()} |
                                      {ok, state(), pos_integer()}.
maybe_trigger_direct_rotation(State) ->
    Started = ?CONFIG(jwt_keys_started, false),
    ?SETCONFIG(jwt_keys_started, true),
    trigger_direct_rotation_if_started(Started, State).

%% @doc this triggers a direct rotation after a crash.
%% as through a crash the tack of time is lost, we just assume it is now.
%% so setting the key rotation time to now and setting timeout to 1 ms.
-spec trigger_direct_rotation_if_started(boolean(), state())
                                   -> {ok, state()} |
                                      {ok, state(), pos_integer()}.
trigger_direct_rotation_if_started(true, BaseState) ->
    Now = current_time(),
    State = BaseState#state{next_key_rotation=Now},
    {ok, State, 1};
trigger_direct_rotation_if_started(_, State) ->
    {ok, State}.

%% @doc handle the calls, only one is supported to perform the inital read.
%% This must always return a timeout to trigger the check.
-spec handle_call(read_key | any(), any(), state())
                 -> {reply, any(), state(), pos_integer()}.
handle_call(read_key, _From, State) ->
    {Result, NewState} = initial_read(State),
    {reply, Result, NewState, ?IDLE_TIMEOUT};
handle_call(_Request, _From, State) ->
    {reply, ignored, State, ?IDLE_TIMEOUT}.

%% @doc handle the casts. This is only stopping.
%% This must always return a timeout to trigger the check.
-spec handle_cast(any(), state())
                 -> {noreply, state(), pos_integer()} |
                    {noreply, state()} |
                    {stop, normal, state()}.
handle_cast(stop, State) ->
    %% stopping, no need for tiemouts anymore
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State, ?IDLE_TIMEOUT}.


%% @doc handle the info. This can only be the update check.
%% This must always return a timeout to trigger the check.
-spec handle_info(timeout | any(), state())
                 -> {noreply, state(), pos_integer()} .
handle_info(timeout, State) ->
    {ok, NewState} = maybe_update(State),
    {noreply, NewState, ?IDLE_TIMEOUT};
handle_info(_Request, State) ->
    {noreply, State, ?IDLE_TIMEOUT}.

%% @doc code change does nothing
-spec code_change(any(), any(), state()) -> {ok, state()}.
code_change(_, _, State) ->
    {ok, State}.

%% @doc terminate logs an error on crash.
-spec terminate(any(), state()) -> ok.
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    lager:error("JWT Keys: terminating with reason ~p", [Reason]),
    ok.

%% @doc perform the initale read and maybe generation of keys.
-spec initial_read(state()) -> {ok, state()} | {error, state()}.
initial_read(#state{key_file = KeyFile, info_file=InfoFile} = State) ->
    KeyFileExists = filelib:is_regular(KeyFile),
    InfoFileExists = filelib:is_regular(InfoFile),
    create_or_read_files(KeyFileExists and InfoFileExists, State).

%% @doc either create the files or read them.
-spec create_or_read_files(boolean(), state()) ->
                                  {ok, state()} |
                                  {error, state()}.
create_or_read_files(true, State) ->
    read_files(State);
create_or_read_files(false, State) ->
    create_files(State).

%% @doc read the key files (key itself and info).
-spec read_files(state()) -> {ok, state()} |
                            {error, state()}.
read_files(#state{key_file = KeyFile, info_file=InfoFile} = State) ->
    KeyResult = read_jwt_key(KeyFile),
    InfoResult = read_key_info(InfoFile),
    Result = handle_read_result(KeyResult, InfoResult, State),
    {Result, update_rot_del(State)}.

%% @doc read the jwt key file
-spec read_jwt_key(binary()) -> {ok, key()} | {error, atom()}.
read_jwt_key(Path) ->
    case watts_file_util:read_pem_entries(Path) of
        [{Type, _, not_encrypted} = KeyData]
          when Type == 'RSAPrivateKey'; Type == 'PrivateKeyInfo'->
            KeyInfo = public_key:pem_entry_decode(KeyData),
            Decoded = KeyInfo#'PrivateKeyInfo'.privateKey,
            Key = public_key:der_decode('RSAPrivateKey', Decoded),
            E = encode_key_value(Key#'RSAPrivateKey'.publicExponent),
            N = encode_key_value(Key#'RSAPrivateKey'.modulus),
            D = encode_key_value(Key#'RSAPrivateKey'.privateExponent),
            JwtKey = #{kty => <<"RSA">>, n => N, e => E, d => D},
            {ok, JwtKey};
        _ ->
            {error, not_readable}
    end.

%% @doc read the info file
-spec read_key_info(binary())
                   -> {ok, map()} | {error, atom()}.
read_key_info(Path) ->
   MaybeData = file:read_file(Path),
   handle_jwt_info_file(MaybeData).

%% @doc handle the result of creating the info file.
-spec handle_jwt_info_file({ok, binary()} | any())
                          -> {ok, map()} | {error, atom()}.
handle_jwt_info_file({ok, Bin}) ->
    {ok, binary_to_term(Bin)};
handle_jwt_info_file(_) ->
    {error, not_readable}.

%% @doc handle the results of the reading of the key/info
-spec handle_read_result(
        {ok, Key :: key()} | any(),
        {ok, #{bits => binary(), generated => integer()}}|any(),
        state()) -> ok | error.
handle_read_result({ok, Key}, {ok, #{bits := Bits, generated := GenTime }},
                   #state{key_bits = ShouldBits} = State) ->
    BitsChanged = (ShouldBits /= Bits),
    regenerate_or_use_key(BitsChanged or too_old(GenTime), Key, State),
    ok;
handle_read_result(_, _, _) ->
    error.

%% @doc use the key or regeneratae one:w
-spec regenerate_or_use_key(boolean(), key(), state())
                           -> ok | error | {ok, state()} | {error, state()}.
regenerate_or_use_key(false, Key, _State) ->
    add_new_key(Key),
    ok;
regenerate_or_use_key(true, _, State) ->
    create_files(State).

%% @doc create the key and the info
-spec create_files(state()) -> {ok, state()} | {error, state()}.
create_files(#state{key_file = KeyFile, key_bits = Bits} = State) ->
    GenResult = generate_jwt_key(KeyFile, Bits),
    handle_key_generation_result(GenResult, State).

%% @doc handle the result of creating the key file and generate info.
-spec handle_key_generation_result(ok | error, state())
                            -> {ok, state()} | {error, state()}.
handle_key_generation_result(ok, #state{info_file = InfoFile,
                                    key_bits = Bits } = State) ->
    Info = #{generated => current_time(),
             bits => Bits},
    ok = file:write_file(InfoFile, term_to_binary(Info)),
    read_files(State);
handle_key_generation_result(error, State) ->
    {error, update_rot_del(State)}.


%% @doc maybe update the current key setting.
%% Either delete old, unused keys or rotate.
-spec maybe_update(state()) -> {ok, state()}.
maybe_update(#state{next_key_delete = NextDelete,
                    next_key_rotation = NextRotation} = State) ->
    perform_update(NextDelete, NextRotation, current_time(), State).

%% @doc perform the needed updates
-spec perform_update(undefined | integer(), undefined | integer(), integer(),
                     state()) -> {ok, state()}.
perform_update(undefined, undefined, _, State) ->
    {ok, State};
perform_update(NextDelete, NextRotation, Now, State)
  when is_integer(NextDelete), Now >= NextDelete ->
    delete_oldest_key(),
    perform_update(undefined, NextRotation, Now, update_delete(State));
perform_update(NextDelete, NextRotation, Now, State)
  when is_integer(NextRotation),  Now >= NextRotation ->
    lager:info("JWT Keys: rotating"),
    create_files(State),
    perform_update(NextDelete, undefined, Now, update_rotation(State));
perform_update(_, _, _, State) ->
    {ok, State}.

%% @doc generate a new rsa private key for jwt signing
-spec generate_jwt_key(KeyFile :: binary(), Bits :: binary()) -> ok | error.
generate_jwt_key(KeyFile, Bits) ->
    case run_openssl(KeyFile, Bits)  of
        {ok, _} ->
            ok;
        {error, _Reason} ->
            error
    end.

%% @doc run the openssl command to create the key file
-spec run_openssl(binary(), binary()) -> {ok, any()} | {error, any()}.
run_openssl(KeyFile, Bits) ->
    Cmd = << <<"openssl genpkey -algorithm rsa -outform PEM">>/binary,
             <<" -pkeyopt rsa_keygen_bits:">>/binary, Bits/binary,
             <<" -out ">>/binary, KeyFile/binary >>,
    exec:run(binary_to_list(Cmd), [sync, stdout, stderr]).




%% @doc convert an unsigned integer in base64 encoded value
-spec encode_key_value(pos_integer()) -> binary().
encode_key_value(Value) ->
    Bin = binary:encode_unsigned(Value),
    base64url:encode(Bin).

%% @doc check if the creation time of the key is too old.
-spec too_old(integer()) -> boolean().
too_old(GenTime) ->
    (current_time() - GenTime) >= ?CONFIG(jwt_key_rotation_interval).

%% @doc drop the tail of the list of keys in use, the oldest one.
-spec delete_oldest_key() -> ok.
delete_oldest_key() ->
    Keys = delete_oldest_key(?CONFIG(jwt_keys)),
    ?SETCONFIG(jwt_keys, Keys),
    ok.

%% @doc ensure that at least one key is left.
-spec delete_oldest_key([key()]) -> [key()].
delete_oldest_key(Keys)
  when length(Keys) > 1 ->
    lager:info("JWT Keys: deleting oldest key"),
    lists:droplast(Keys);
delete_oldest_key(Keys) ->
    Keys.

%% @doc add a new key to the list of keys.
-spec add_new_key(key()) -> ok.
add_new_key(NewKey) ->
    OldKeys = ?CONFIG(jwt_keys, []),
    ?SETCONFIG(jwt_keys, [NewKey | OldKeys]),
    ok.

%% @doc get the current time in seconds.
-spec current_time() -> integer().
current_time() ->
    erlang:system_time(seconds).

%% @doc calculate the times in seconds of the next key rotation.
-spec next_key_rot() -> integer().
next_key_rot() ->
    ?CONFIG(jwt_key_rotation_interval) + current_time().

%% @doc calculate the times in seconds of the next key dropping.
-spec next_key_del() -> integer().
next_key_del() ->
    current_time()+round(?CONFIG(session_timeout, 10000)/1000).

%% @doc update the time of next key deletion in the state
-spec update_delete(state()) -> state().
update_delete(State) ->
    State#state{
      next_key_delete = next_key_del()
     }.

%% @doc update the time of next key rotation in the state
-spec update_rotation(state()) -> state().
update_rotation(State) ->
    State#state{
      next_key_rotation = next_key_rot()
     }.

%% @doc update both, key delete and rotation time.
-spec update_rot_del(state()) -> state().
update_rot_del(State) ->
    update_rotation(update_delete(State)).
