-module(tts_data). 


-export([init/0]).

-export([
         sessions_create_new/1,
         sessions_get_pid/1,
         sessions_update_pid/2,
         sessions_delete/1,
         sessions_inspect/0
        ]).

-export([
         user_create_new/2,
         user_get_pid/1,
         user_delete/1,
         user_delete_pid/1,
         user_inspect/0
        ]).

-export([
         oidc_add_op/5,
         oidc_op_set/3,
         oidc_get_op/1,
         oidc_get_op_list/0,
         oidc_op_inspect/0
        ]).

-define(TTS_SESSIONS,tts_sessions).
-define(TTS_OIDCP,tts_oidcp).
-define(TTS_USER,tts_user).

-define(TTS_TABLES,[
                    ?TTS_SESSIONS,
                    ?TTS_OIDCP,
                    ?TTS_USER
                   ]).

init() ->
    create_tables().

% functions for session management
-spec sessions_create_new(ID :: binary()) -> ok | {error, Reason :: atom()}.
sessions_create_new(ID) ->
    return_ok_or_error(insert_new(?TTS_SESSIONS,{ID,none_yet})).


-spec sessions_get_pid(ID :: binary()) -> {ok, Pid :: pid()} | {error, Reason :: atom()}.
sessions_get_pid(ID) ->
    validate_pid_value(lookup(?TTS_SESSIONS, ID)).

-spec sessions_update_pid(ID :: binary(), Pid :: pid()) -> ok.
sessions_update_pid(ID,Pid) ->
    insert(?TTS_SESSIONS,{ID, Pid}),
    ok.    

-spec sessions_delete(ID :: binary()) -> true.
sessions_delete(ID) ->
   delete(?TTS_SESSIONS,ID). 

-spec sessions_inspect() -> ok. 
sessions_inspect() ->
    iterate_through_table_and_print(?TTS_SESSIONS).

% functions for user management 

-spec user_create_new(ID :: binary(), Pid::pid()) -> ok | {error, Reason :: atom()}.
user_create_new(Id,Pid) ->
    return_ok_or_error(insert_new(?TTS_USER,[{Id,Pid},{Pid,Id}])).


-spec user_get_pid(ID :: binary()) -> {ok, Pid :: pid()} | {error, Reason :: atom()}.
user_get_pid(Id) ->
    validate_pid_value(lookup(?TTS_USER,Id)).

-spec user_delete(ID :: binary()) -> true.
user_delete(Id) ->
    delete_kv_vk(?TTS_USER,Id).

-spec user_delete_pid(Pid :: pid()) -> true.
user_delete_pid(Pid) ->
    delete_kv_vk(?TTS_USER,Pid).

-spec user_inspect() -> ok. 
user_inspect() ->
    iterate_through_table_and_print(?TTS_USER).



% functions for oidc management
-spec oidc_add_op(Identifier::binary(), Description::binary(), ClientId ::
                  binary(), ClientSecret:: binary(), ConfigEndpoint:: binary()) ->ok | {error, Reason :: atom()}.
oidc_add_op(Identifier, Description, ClientId, ClientSecret, ConfigEndpoint) ->
    Map = #{
      id => Identifier,
      desc => Description,
      client_id => ClientId,
      client_secret => ClientSecret,
      config_endpoint => ConfigEndpoint
     },
    return_ok_or_error(insert_new(?TTS_OIDCP,{Identifier,Map})).


-spec oidc_op_set(Key::atom(), Value::any(), Identifier::binary()) ->ok.
oidc_op_set(Key, Value, Id) ->
    {ok, {Id,Map0}} = oidc_get_op(Id),
    Map = maps:put(Key, Value, Map0),
    return_ok_or_error(insert(?TTS_OIDCP,{Id,Map})).

-spec oidc_get_op(RemoteEndpoint :: binary()) -> {ok, Info :: map()} | {error, Reason :: atom()}.
oidc_get_op(RemoteEndpoint) ->
    lookup(?TTS_OIDCP,RemoteEndpoint). 

-spec oidc_get_op_list() -> [map()].
oidc_get_op_list() ->
    Entries = get_all_entries(?TTS_OIDCP),
    ExtractValue = fun({_, Val}, List) ->
                           [Val | List]
                   end,
    lists:reverse(lists:foldl(ExtractValue,[],Entries)).

-spec oidc_op_inspect() -> ok. 
oidc_op_inspect() ->
    iterate_through_table_and_print(?TTS_OIDCP).

%% internal functions


return_ok_or_error(true) ->
    ok;
return_ok_or_error(false) ->
    {error, already_used}.

validate_pid_value({ok,{_Id,Pid}}) when is_pid(Pid) ->
    {ok, Pid};
validate_pid_value({ok,{_Id,_Pid}}) ->
    {error, not_set};
validate_pid_value({error, _} = Error) ->
    Error.


create_tables() ->
  CreateTable = fun(Table) ->
                    ets:new(Table,[set, public, named_table, {keypos,1}])
                end,
  lists:map(CreateTable,?TTS_TABLES),
  ok.


get_all_entries(Table) -> 
    GetVal = fun(Entry,List) ->
                [Entry | List]
             end,
    Entries = ets:foldl(GetVal,[],Table),
    lists:reverse(Entries).

iterate_through_table_and_print(Table) ->
    First = ets:first(Table),
    io:format("=== Table: ~p ===~n",[Table]),
    iterate_through_table_and_print(Table, First).

iterate_through_table_and_print(_Table, '$end_of_table') ->
    io:format("=== END ===~n",[]),
    ok;
iterate_through_table_and_print(Table, Key) ->
    {ok, Entry} = lookup(Table,Key),
    io:format("=> ~p ~n",[Entry]),
    Next = ets:next(Table,Key),
    iterate_through_table_and_print(Table,Next).
    
delete(Table, Key) ->
    true = ets:delete(Table,Key).

delete_kv_vk(Table, Key) ->
    case ets:lookup(Table,Key) of
        [{Key,Value}] ->
            delete(Table, Key),
            delete(Table, Value);
        [] ->
            {error, not_found};
        _ ->
            {error, to_many}
    end.



insert(Table, Entry) ->
    true = ets:insert(Table, Entry).

insert_new(Table, Entry) ->
    ets:insert_new(Table, Entry).

lookup(Table, Key) ->
  create_lookup_result(ets:lookup(Table, Key)).

create_lookup_result([Element]) ->
    {ok, Element};
create_lookup_result([]) ->
    {error, not_found};
create_lookup_result(_) ->
    {error, too_many}.
