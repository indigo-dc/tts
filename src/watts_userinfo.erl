%% @doc This module implements the userinfo data structure and the functions
%% to access it
-module(watts_userinfo).
%%
%% Copyright 2016 - 2017 SCC/KIT
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

-export([
         new/0,
         update_with_token/2,
         update_iss_sub/3,
         update_id_token/2,
         update_access_token/2,
         update_id_info/2,
         update_token_info/2,
         add_additional_login/4,
         clear_additional_logins/2,
         has_additional_login/3,
         return/2
        ]).

-export_type([userinfo/0]).

-record(user_info, {
          issuer = undefined :: undefined | binary(),
          subject = undefined :: undefined | binary(),
          id_token = #{} :: map(),
          id_info = #{} :: map(),
          access_token = #{} :: map(),
          plugin_info = #{} :: map(),
          token_info = #{} :: map(),
          additional_logins = [] :: [additional_login()],
          scope = #{} :: map()
         }).

-type userinfo() :: #user_info{}.
-type additional_login() :: {{ServiceId :: binary(),
                              IssuerId :: binary()}, UserInfo :: userinfo()}.

%% @doc create a new userinfo structure (record).
-spec new() -> {ok, userinfo()}.
new() ->
    {ok, #user_info{}}.


%% @doc update the structure with the information from the Token
-spec update_with_token(Token :: map(), UserInfo :: userinfo()) ->
                               {ok, UpdatedInfo :: userinfo()}.
update_with_token(Token, UserInfo) ->
    IdInfo = maps:get(user_info, Token, undefined),
    IdToken = maps:get(id, Token, undefined),
    AccToken = maps:get(access, Token, undefined),
    Scope = maps:get(scope, Token, undefined),
    TokenInfo = maps:get(token_info, Token, undefined),

    Info1 =
        case IdToken of
            undefined ->
                UserInfo;
            _ ->
                {ok, Inf1} = update_id_token(IdToken, UserInfo),
                Inf1
        end,
    Info2 =
        case IdInfo of
            undefined ->
                Info1;
            _ ->
                {ok, Inf2} = update_id_info(IdInfo, Info1),
                Inf2
        end,
    Info3 =
        case AccToken of
            undefined ->
                Info2;
            _ ->
                {ok, Inf3} = update_access_token(AccToken, Info2),
                Inf3
        end,
    Info4 =
        case TokenInfo of
            undefined ->
                Info3;
            _ ->
                {ok, Inf4} = update_token_info(TokenInfo, Info3),
                Inf4
        end,
    Info5 =
        case Scope of
            undefined ->
                Info4;
            _ ->
                {ok, Inf5} = update_scope(Scope, Info4),
                Inf5
        end,
    {ok, Info5}.

%% @doc update issuer and subject of the userinfo, if valid.
-spec update_iss_sub(Issuer :: binary() | undefined,
                     Subject :: binary() | undefined,
                     UserInfo :: userinfo()) ->
                            {ok, UpdatedInfo :: userinfo()} |
                            {error, Reason :: atom()}.
update_iss_sub(Issuer, Subject,
               #user_info{issuer=undefined, subject=undefined} = Info)
  when is_binary(Issuer), is_binary(Subject)->
    {ok, update_plugin_info(Info#user_info{issuer=Issuer, subject=Subject})};
update_iss_sub(Issuer, Subject,
               #user_info{issuer=Issuer, subject=Subject} = Info)
  when is_binary(Issuer), is_binary(Subject)->
    {ok, Info};
update_iss_sub(undefined, Subject, #user_info{subject=Subject} = Info)
  when is_binary(Subject)->
    {ok, Info};
update_iss_sub(Issuer, undefined, #user_info{issuer=Issuer} = Info)
  when is_binary(Issuer)->
    {ok, Info};
update_iss_sub(undefined, undefined, Info) ->
    {ok, Info};
update_iss_sub(_Issuer, _Subject, _Info) ->
    {error, bad_iss_sub}.


%% @doc claims and iss/sub coming from an Id Token
-spec update_id_token(IdToken :: map(), Info ::userinfo())
                     ->  {ok, userinfo()} | {error, not_match}.
update_id_token(IdToken, Info) ->
    Claims = maps:get(claims, IdToken, #{}),
    Iss = maps:get(iss, Claims),
    Sub = maps:get(sub, Claims),
    case update_iss_sub(Iss, Sub, Info) of
        {ok, NewInfo} ->
            {ok, update_plugin_info(NewInfo#user_info{id_token = IdToken})};
        {error, bad_iss_sub} ->
            {error, not_match}
    end.


%% @doc update the access token in the userinfo, if it is valid.
-spec update_access_token(TokenMap :: map(), UserInfo :: userinfo()) ->
                            {ok, UpdatedInfo :: userinfo()} |
                            {error, Reason :: atom()}.
update_access_token(#{token := Token} = AccessToken,
                    #user_info{id_token = IdToken}=Info) ->
    Claims = maps:get(claims, IdToken, #{}),
    Hash = maps:get(at_hash, Claims, undefined),
    << BinHash:16/binary, _Rest/binary>> = crypto:hash(sha256, Token),
    AtHash =  base64url:encode(BinHash),
    case Hash of
        undefined ->
            {ok, Info#user_info{access_token=AccessToken}};
         AtHash ->
            {ok, Info#user_info{access_token=AccessToken}};
        _ ->
            {error, not_match}
    end.

%% @doc update subject from the Userinfo Endpoint, if matching the userinfo.
-spec update_id_info(IdInfo :: map(), UserInfo :: userinfo()) ->
                            {ok, UpdatedInfo :: userinfo()} |
                            {error, Reason :: atom()}.
update_id_info(IdInfo, Info) ->
    Sub = maps:get(sub, IdInfo),
    Iss = undefined,
    case update_iss_sub(Iss, Sub, Info) of
        {ok, NewInfo} ->
            {ok, update_plugin_info(NewInfo#user_info{id_info=IdInfo})};
        {error, bad_iss_sub} ->
            {error, not_match}
    end.

%% @doc update subject from the Tokeninfo Endpoint and set scopes.
-spec update_token_info(TokenInfo :: map(), UserInfo :: userinfo()) ->
                               {ok, UpdatedInfo :: userinfo()} |
                               {error, Reason :: atom()}.
update_token_info(TokenInfo, Info) ->
    Sub = maps:get(sub, TokenInfo, undefined),
    Iss = undefined,
    case update_iss_sub(Iss, Sub, Info) of
        {ok, NewInfo} ->
            {ok, update_plugin_info(
                   set_scope_if_empty(
                     NewInfo#user_info{token_info=TokenInfo}
                    )
                  )
            };
        {error, bad_iss_sub} ->
            {error, not_match}
    end.

%% @doc set the scope in the userinfo
-spec update_scope(Scope :: map(), UserInfo :: userinfo()) ->
                          {ok, UpdatedInfo :: userinfo()}.
update_scope(#{scope := _, list := _} =Scope, Info) ->
    {ok, update_plugin_info(Info#user_info{scope=Scope})}.


%% @doc add an additional login
-spec add_additional_login(ServiceId :: binary(), IssuerId :: binary(),
                           Token :: map(), UserInfo :: userinfo()) ->
                                  UpdatedInfo :: userinfo().
add_additional_login(ServiceId, IssuerId, Token,
                     #user_info{additional_logins=AddLogins} = Info) ->
    {ok, EmptyInfo} = new(),
    {ok, UserInfo} = update_with_token(Token, EmptyInfo),
    NewEntry = {{ServiceId, IssuerId}, UserInfo},
    NewAddLogins = [ NewEntry |
                     lists:keydelete({ServiceId, IssuerId}, 1, AddLogins)],
    Info#user_info{additional_logins = NewAddLogins}.


%% @doc delete all additional logins for a service
-spec clear_additional_logins(ServiceId :: binary(), UserInfo :: userinfo())
                             -> UpdatedInfo :: userinfo().
clear_additional_logins(ServiceId,
                       #user_info{additional_logins=AddLogins} = Info) ->
    Filter = fun({{Id, _}, _}) ->
                     Id /= ServiceId
             end,
    NewAddLogins = lists:filter(Filter, AddLogins),
    Info#user_info{additional_logins = NewAddLogins}.


%% @doc check if an additional login for the service and issuer exists.
-spec has_additional_login(ServiceId :: binary(), IssuerId :: binary(),
                           UserInfo :: userinfo()) -> boolean().
has_additional_login(ServiceId, IssuerId,
                     #user_info{additional_logins=AddLogins}) ->
    case lists:keyfind({ServiceId, IssuerId}, 1, AddLogins) of
        false ->
            false;
        {{ServiceId, IssuerId}, _} ->
            true
    end.


%% @doc generic function to return needed information.
%% The returned values are:
%% <ul>
%% <il> {key, Key}: get a certain key from the plugin info, used for service
%% authz </il>
%% <il> plugin_info: get the plugin info map </il>
%% <il> additional_logins: get the additional logins </il>
%% <il> issuer_subject: get the Issuer and Subject  </il>
%% <il> issuer: get the issuer used to login </il>
%% <il> subject: get the subject of the user </il>
%% <il> id: return the WaTTS userid </il>
%% <il> access_token: return the access token </il>
%% <il> display_name: return the display name of the user </il>
%% <il> logged_in: return if the user is logged in </il>
%% </ul>
-spec return(Key :: atom() |
                    {key, Key :: binary()} |
                    {additional_logins, ServiceId :: binary(),
                     WithAccessToken :: boolean()}
            , UserInfo :: userinfo()) ->
                    {ok, Data :: any()} | {ok, Iss :: binary(), Sub :: binary}
                        | boolean() | {error, Reason :: atom()}.
return({key, Key0}, #user_info{plugin_info=PluginInfo}) ->
    Key = maybe_to_atom(Key0),
    case maps:is_key(Key, PluginInfo) of
        true ->
            {ok, maps:get(Key, PluginInfo)};
        false ->
            {error, not_found}
    end;
return(plugin_info, #user_info{plugin_info=PluginInfo}) ->
    {ok, PluginInfo};
return({additional_logins, ServiceId, WithAccessToken},
       #user_info{additional_logins = AddLogins}) ->
    Extract = fun({{SrvId, _}, Info}, List) when SrvId == ServiceId ->
                      {ok, UInfo} = return(plugin_info, Info),
                      {ok, PAccT} = return(access_token, Info),
                      Base = #{user_info => UInfo},
                      Update =
                          case WithAccessToken of
                              true ->
                                  #{access_token => PAccT};
                              false ->
                                  #{}
                          end,

                      [ maps:merge(Base, Update) | List];
                 (_, List) ->
                      List
              end,
    AddLoginInfo = lists:foldl(Extract, [], AddLogins),
    {ok, AddLoginInfo};
return(issuer_subject, Info) ->
    {ok, Issuer} = return(issuer, Info),
    {ok, Subject} = return(subject, Info),
    {ok, Issuer, Subject};
return( subject, #user_info{subject=Subject}) ->
    {ok, Subject};
return( issuer, #user_info{issuer=Issuer}) ->
    {ok, Issuer};
return( id, Info) ->
    userid(Info);
return( access_token, Info) ->
    access_token(Info);
return( display_name, Info) ->
    display_name(Info);
return( logged_in, Info) ->
    logged_in(Info).


%% @doc generate the userid
-spec userid(UserInfo :: userinfo()) -> {ok, Id :: binary} |
                                        {error, Reason :: atom()}.
userid(#user_info{issuer=Issuer, subject=Subject})
  when is_binary(Issuer), is_binary(Subject) ->
    Id = base64url:encode(jsone:encode(#{issuer => Issuer,
                                         subject => Subject})),
    {ok, Id};
userid(_) ->
    {error, not_set}.

%% @doc generate the display name
-spec display_name(UserInfo :: userinfo()) -> {ok, Name :: binary} |
                                        {error, Reason :: atom()}.
display_name(#user_info{subject=Subject, issuer=Issuer, id_info=IdInfo})
  when is_binary(Subject), is_binary(Issuer)->
    case maps:get(name, IdInfo, undefined) of
        undefined -> {ok, << Subject/binary, <<"@">>/binary, Issuer/binary >>};
        Other -> {ok, Other}
    end;
display_name(_) ->
    {error, not_set}.

%% @doc return if the user is logged in
-spec logged_in(UserInfo :: userinfo()) -> boolean().
logged_in(#user_info{subject=Subject, issuer=Issuer})
  when is_binary(Subject), is_binary(Issuer)->
    true;
logged_in(_) ->
    false.

%% @doc return the access token, if present
-spec access_token(UserInfo :: userinfo()) -> {ok, AccessToken :: binary} |
                                        {error, Reason :: atom()}.
access_token(#user_info{access_token=#{token := AccessToken}}) ->
    {ok, AccessToken};
access_token(_) ->
    {error, not_set}.

%% @doc set an empty scope if no scope is set
-spec set_scope_if_empty(UserInfo :: userinfo()) -> UpdatedInfo :: userinfo().
set_scope_if_empty(#user_info{token_info=TokenInfo, scope= ScopeMap} = Info) ->
    case maps:is_key(scope, ScopeMap) of
        true ->
            Info;
        false ->
            Empty = #{scope => <<"">>, list => []},
            Info#user_info{scope = maps:get(scope, TokenInfo, Empty)}
    end.

%% @doc update the prepared information for the plugin about the user.
-spec update_plugin_info(UserInfo :: userinfo()) -> UpdatedInfo :: userinfo().
update_plugin_info(#user_info{id_info=IdInfo, id_token=IdToken,
                              token_info=TokenInfo,
                              issuer=Issuer, subject=Subject,
                              scope= ScopeMap
                             } = UserInfo) ->
    RemoveClaims = [aud, exp, nbf, iat, jti, azp, kid, aud, auth_time, at_hash,
                    c_hash],
    ReducedClaims = maps:without(RemoveClaims, maps:get(claims, IdToken, #{})),
    RemoveTokenInfo = [active, aud, sub, iss, client_id, token_type, exp, iat,
                       nbf, jti, scope],
    ReducedTokenInfo = maps:without(RemoveTokenInfo, TokenInfo),
    ScopeInfo = case maps:get(scope, ScopeMap, undefined) of
                    undefined ->
                        #{};
                    Scope ->
                        ScopeList = maps:get(list, ScopeMap),
                        #{scope_list => ScopeList, scope => Scope}
                end,
    IssSubUpdate = #{iss => Issuer, sub=> Subject},
    PluginInfo1 = maps:merge(IdInfo, ReducedClaims),
    PluginInfo2 = maps:merge(PluginInfo1, ReducedTokenInfo),
    PluginInfo3 = maps:merge(PluginInfo2, ScopeInfo),
    PluginInfo9 = maps:merge(PluginInfo3, IssSubUpdate),
    UserInfo#user_info{plugin_info=PluginInfo9}.

%% @doc convert a binary to atom, if the atom exists
-spec maybe_to_atom(Binary :: binary()) -> atom() | binary().
maybe_to_atom(Bin) ->
    try
        binary_to_existing_atom(Bin, utf8)
    catch _:_ ->
            Bin
    end.
