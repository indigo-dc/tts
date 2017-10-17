
%% @doc this is the management module for the temporary credential data storage.
%% The credentials are only stored in RAM and each in a separate process.
%%
%% This module is running as a registered process so it can be reached without
%% knowing its process id. It sets up the API to store and retrieve credentials.
%%
%% @see watts_temp_cred_data
-module(watts_temp_cred).
%%
%% Copyright 2016 SCC/KIT
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

-behaviour(gen_server).
-include("watts.hrl").

%% API.
-export([start_link/0]).
-export([add_cred/2]).
-export([get_cred/2]).
-export([get_all_creds/0]).
-export([exists/2]).
-export([stop/0]).


%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          creds=[] :: [cred_record()]
         }).

-type cred_record() :: {Id :: binary() ,
                        UserId :: binary(),
                        DataPid :: pid(),
                        MRef :: reference()}.
-type state() :: #state{}.
%% API.

%% @doc start the gen_server linked
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, noparams, []).

%% @doc add a credential, this creates a new temp_cred_data process.
-spec add_cred(Credential :: watts:credential(), UserId::binary())
              -> {ok, Id::binary()}.
add_cred(Credential, UserId) ->
    gen_server:call(?MODULE, {add, Credential, UserId}).

%% @doc retrieve the data if the userid is the same
-spec get_cred(CredId :: binary(), UserId :: binary()) ->
                      {ok, Credential :: watts:credential()} |
                      {error, Reason :: atom()}.
get_cred(Id, UserId) ->
    gen_server:call(?MODULE, {get, Id, UserId}).

%% @doc check if a Credential of id exists for given user.
-spec exists(CredId :: binary(), UserId::binary()) -> boolean().
exists(Id, UserId) ->
    gen_server:call(?MODULE, {exists, Id, UserId}).

%% @doc DO NOT USE! get a list of all credentials *ONLY* for testing!
-spec get_all_creds() -> {ok, Creds::list()}.
get_all_creds() ->
    gen_server:call(?MODULE, get_all_creds).

%% @doc stop the process.
-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

%% gen_server.
%% @doc initialize the gen_server with an empty state.
-spec init(noparams)  -> {ok, state()}.
init(noparams) ->
    {ok, #state{}}.

%% @doc handle the gen_server:call
-spec handle_call(Msg :: any(), From :: any(), State :: state()) ->
                         {reply, Result :: any(), NewState :: state()}.
handle_call({add, Cred, UserId}, _From, State) ->
    {ok, Id, NewState} = add_credential(Cred, UserId, State),
    {reply, {ok, Id}, NewState};
handle_call({exists, Id, UserId}, _From, State) ->
    Result = credential_exists(Id, UserId, State),
    {reply, Result, State};
handle_call({get, Id, UserId}, _From, State) ->
    Result = get_credential(Id, UserId, State),
    {reply, Result, State};
handle_call(get_all_creds, _From, #state{creds=Creds} = State) ->
    {reply, {ok, Creds}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


%% @doc store a new credential, ensure to have a unique id.
-spec add_credential(Credential :: watts:credential(),
                     UserId :: binary(),
                     State :: state())
                    -> {ok, CredentialId :: binary(), NewState :: state()}.
add_credential(Cred, UserId, #state{creds = Creds} = State) ->
    Id = gen_random_id(State),
    {ok, DataPid} = watts_temp_cred_sup:new_temp_cred(Cred),
    MRef = monitor(process, DataPid),
    NewCreds = [{Id, UserId, DataPid, MRef} | Creds],
    {ok, Id, State#state{creds = NewCreds}}.


%% @doc generate a random unique credential id
-spec gen_random_id(state()) -> binary().
gen_random_id(#state{creds = Creds} = State) ->
    Id = watts_utils:random_string(16),
    case lists:keyfind(Id, 1, Creds) of
        false -> Id;
        _ -> gen_random_id(State)
    end.

%% @doc check if a credential for the user exists
-spec credential_exists(CredentiaId :: binary(),
                        UserId :: binary(),
                        State :: state()) -> boolean().
credential_exists(Id, UserId, State) ->
    case get_cred_pid(Id, UserId, State) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

%% @doc get a crecential for a user.
-spec get_credential(CredentiaId :: binary(),
                     UserId :: binary(),
                     State :: state())
                    -> {ok, Credential :: watts:credential()} |
                       {error, not_found}.
get_credential(Id, UserId, State) ->
    case get_cred_pid(Id, UserId, State) of
        {ok, Pid} ->
            watts_temp_cred_data:get_credential(Pid);
        _ ->
            {error, not_found}
    end.

%% @doc get the pid of the data process.
-spec get_cred_pid(CredentiaId :: binary(),
                     UserId :: binary(),
                     State :: state())
                    -> {ok, Pid :: pid()} |
                       {error, not_found}.
get_cred_pid(Id, UserId, #state{creds=Creds}) ->
    case lists:keyfind(Id, 1, Creds) of
        {Id, UserId, CredPid, _MRef} ->
            {ok, CredPid};
        _ ->
            {error, not_found}
    end.


%% @doc only support the stop cast
-spec handle_cast(any(), state())
                 -> {stop, normal, state()} | {noreply, state()}.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc only support the Down info for a data process
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({'DOWN', _MRef, process, Pid, _Info}, #state{creds=Creds}=State) ->
    NewCreds = lists:keydelete(Pid, 3, Creds),
    {noreply, State#state{creds = NewCreds}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc implementation of terminate, does nothing
-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc code_change implementation, does nothing
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
