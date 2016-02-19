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
         user_lookup_info/2,
         user_insert_info/2,
         user_delete_info/1,
         user_delete_info/2,
         user_delete_entries_older_than/1,
         user_delete_entries_not_accessed_for/1,
         user_inspect/0,
         user_mapping_inspect/0,

         insert_users/2
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
-define(TTS_USER_MAPPING,tts_user_mapping).

-define(TTS_TABLES,[
                    ?TTS_SESSIONS,
                    ?TTS_OIDCP,
                    ?TTS_USER_MAPPING,
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
    return_value(lookup(?TTS_SESSIONS, ID)).

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

-spec user_lookup_info(Issuer::binary(), Subject::binary()) -> {ok, Info :: map()} | {error, Reason :: atom()}.
user_lookup_info(Issuer,Subject) ->
    user_get_info(return_value(lookup(?TTS_USER_MAPPING,{Issuer,Subject}))).


-spec user_insert_info(Info::map(), MaxEntries::integer())  -> ok | {error, Reason :: atom()}.
user_insert_info(Info,MaxEntries) ->
    CurrentEntries = ets:info(?TTS_USER,size), 
    remove_unused_entries_if_needed(CurrentEntries,MaxEntries),
    IssSubList = maps:get(user_ids,Info,[]),
    UserId = maps:get(uid, Info),
    Mappings = [ {IssSub, UserId} || IssSub <- IssSubList ],
    CTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    Tuple = {UserId, Info, CTime, CTime},
    case  insert_new(?TTS_USER,Tuple) of 
        true ->
            return_ok_or_error(insert_new(?TTS_USER_MAPPING,Mappings));
        false ->
            {error, already_exists}
    end.

user_delete_info(Issuer, Subject) -> 
    case lookup(?TTS_USER_MAPPING,{Issuer,Subject}) of
        {ok, UserId} ->
            user_delete_info(UserId);
        {error, _} = Error -> 
            Error
    end.

user_delete_info(#{user_ids := UserIds, uid := UserId}) ->
    user_delete_mappings(UserIds),
    delete(?TTS_USER,UserId),
    ok;
user_delete_info(UserId) ->
    case lookup(?TTS_USER,UserId) of
        {ok, UserInfo} -> user_delete_info(UserInfo);
        {error, _} = Error -> Error
    end.

-spec user_delete_entries_older_than(Duration::number()) -> 
    {ok, NumDeleted::number()}.
user_delete_entries_older_than(Duration) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    iterate_through_users_and_delete_before(ctime,Now-Duration).

-spec user_delete_entries_not_accessed_for(Duration::number()) -> 
    {ok, NumDeleted::number()}.
user_delete_entries_not_accessed_for(Duration) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    iterate_through_users_and_delete_before(atime,Now-Duration).

-spec user_inspect() -> ok. 
user_inspect() ->
    iterate_through_table_and_print(?TTS_USER).

-spec user_mapping_inspect() -> ok. 
user_mapping_inspect() ->
    iterate_through_table_and_print(?TTS_USER_MAPPING).

user_get_info({ok, Id}) ->
    ATime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    ets:update_element(?TTS_USER,Id,{3,ATime}),
    return_value(lookup(?TTS_USER,Id));
user_get_info({error,E}) ->
    {error,E}.

remove_unused_entries_if_needed(CurrentEntries, MaxEntries) 
  when is_integer(CurrentEntries), is_integer(MaxEntries), CurrentEntries >= MaxEntries ->
    Number = 1 + (CurrentEntries - MaxEntries),
    iterate_through_users_and_delete_least_used_ones(Number);
remove_unused_entries_if_needed(_Current, _Max) ->
    ok.


iterate_through_users_and_delete_least_used_ones(Number) ->
    ets:safe_fixtable(?TTS_USER,true),
    First = ets:first(?TTS_USER),
    iterate_through_users_and_delete_least_used_ones(Number,First,[]).
    
iterate_through_users_and_delete_least_used_ones(_Number, '$end_of_table', List) ->
    ets:safe_fixtable(?TTS_USER,false),
    DeleteID = fun({Info,_},_)  ->
                       user_delete_info(Info)
               end,
    lists:foldl(DeleteID,[],List),
    ok;
iterate_through_users_and_delete_least_used_ones(0, Key, [H | T] = List) ->
    {_,LRU_Max } = H,
    {ok, {_UserId, Info, ATime, _CTime}} = lookup(?TTS_USER,Key),
    NewList = case ATime < LRU_Max of
                  true -> 
                      % if the ATime is older than the 
                      % newest on the list
                      lists:reverse(lists:keysort(2,[{Info,ATime}|T]));
                  false -> List
              end,
    Next = ets:next(?TTS_USER,Key),
    iterate_through_users_and_delete_least_used_ones(0, Next,NewList);
iterate_through_users_and_delete_least_used_ones(Number, Key, List) ->
    {ok, {_UserId, Info, ATime, _CTime}} = lookup(?TTS_USER,Key),
    % the newest Item is at the head, as it has the biggest ATime value
    NewList = lists:reverse(lists:keysort(2,[{Info, ATime} | List])),
    Next = ets:next(?TTS_USER,Key),
    iterate_through_users_and_delete_least_used_ones(Number-1, Next,NewList).
    
    


iterate_through_users_and_delete_before(TimeType,TimeStamp) ->
    ets:safe_fixtable(?TTS_USER,true),
    First = ets:first(?TTS_USER),
    iterate_through_users_and_delete_before(TimeType,TimeStamp,First,0).
iterate_through_users_and_delete_before(_TimeType,_TimeStamp,'$end_of_table',NumDeleted) ->
    ets:safe_fixtable(?TTS_USER,false),
    {ok,NumDeleted};
iterate_through_users_and_delete_before(TimeType,TimeStamp,Key,Deleted) ->
    {ok, User} = lookup(?TTS_USER,Key),
    NewDeleted = case delete_user_if_before(TimeType,TimeStamp,User) of
                     true -> Deleted + 1;
                     false -> Deleted
                 end,
    Next = ets:next(?TTS_USER,Key),
    iterate_through_users_and_delete_before(TimeType,TimeStamp,Next,NewDeleted).

delete_user_if_before(ctime,Timestamp,{_UserId,UserInfo,_ATime,CTime}) ->
    delete_user_if_true(Timestamp > CTime, UserInfo);
delete_user_if_before(atime,Timestamp,{_UserId,UserInfo,ATime,_CTime}) ->
    delete_user_if_true(Timestamp > ATime, UserInfo).

delete_user_if_true(true,UserInfo) ->
    user_delete_info(UserInfo),
    true;
delete_user_if_true(_,_UserInfo) ->
    false.

user_delete_mappings([]) ->
    true;
user_delete_mappings([H | T]) ->
    delete(?TTS_USER_MAPPING,H),
    user_delete_mappings(T).

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
    {error, already_exists}.

return_value({ok,{_Key,Value}}) ->
    {ok, Value};
return_value({ok,{_Key,Value,_ATime,_CTime}}) ->
    {ok, Value};
return_value({error, _} = Error) ->
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

insert_users(0,_Max) ->
    ok;
insert_users(Number,Max) ->
    GidUid = Number+2000,
    BinNumber = integer_to_binary(Number),
    User = #{ uid => Number,
              user_ids => [{<<"https://self.here.at.tts">>,integer_to_binary(Number)}],
              uidNumber => GidUid,
              gidNumber => GidUid,
              homeDirectory => << <<"/home/">>/binary, BinNumber/binary >>
            },
    user_insert_info(User,Max),
    insert_users(Number-1,Max).
      

