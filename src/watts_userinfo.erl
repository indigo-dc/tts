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
         has_additional_login/3,
         return/2
        ]).

-record(user_info, {
          issuer = undefined,
          subject = undefined,
          id_token = #{},
          id_info = #{},
          access_token = #{},
          plugin_info = #{},
          token_info = #{},
          additional_logins = []
         }).

new() ->
    {ok, #user_info{}}.


update_with_token(Token, UserInfo) ->
    IdInfo = maps:get(user_info, Token, undefined),
    IdToken = maps:get(id, Token, undefined),
    AccToken = maps:get(access, Token, undefined),
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
    {ok, Info4}.


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

update_id_info(IdInfo, Info) ->
    Sub = maps:get(sub, IdInfo),
    Iss = undefined,
    case update_iss_sub(Iss, Sub, Info) of
        {ok, NewInfo} ->
            {ok, update_plugin_info(NewInfo#user_info{id_info=IdInfo})};
        {error, bad_iss_sub} ->
            {error, not_match}
    end.

update_token_info(TokenInfo, Info) ->
    Sub = maps:get(sub, TokenInfo, undefined),
    Iss = undefined,
    case update_iss_sub(Iss, Sub, Info) of
        {ok, NewInfo} ->
            {ok, update_plugin_info(NewInfo#user_info{token_info=TokenInfo})};
        {error, bad_iss_sub} ->
            {error, not_match}
    end.


add_additional_login(ServiceId, IssuerId, Token,
                     #user_info{additional_logins=AddLogins} = Info) ->
    {ok, EmptyInfo} = new(),
    {ok, UserInfo} = update_with_token(Token, EmptyInfo),
    NewEntry = {{ServiceId, IssuerId}, UserInfo},
    NewAddLogins = [ NewEntry |
                     lists:keydelete({ServiceId, IssuerId}, 1, AddLogins)],
    Info#user_info{additional_logins = NewAddLogins}.


has_additional_login(ServiceId, IssuerId,
                     #user_info{additional_logins=AddLogins}) ->
    case lists:keyfind({ServiceId, IssuerId}, 1, AddLogins) of
        false ->
            false;
        _ ->
            true
    end.

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
return({additional_logins, ServiceId, AddAccessToken},
       #user_info{additional_logins = AddLogins}) ->
    Extract = fun({{SrvId, _}, Info}, List) when SrvId == ServiceId ->
                      {ok, UInfo} = return(plugin_info, Info),
                      {ok, PAccT} = return(access_token, Info),
                      Base = #{user_info => UInfo},
                      Update =
                          case AddAccessToken of
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



userid(#user_info{issuer=Issuer, subject=Subject})
  when is_binary(Issuer), is_binary(Subject) ->
    Id = base64url:encode(jsone:encode(#{issuer => Issuer,
                                         subject => Subject})),
    {ok, Id};
userid(_) ->
    {error, not_set}.

display_name(#user_info{subject=Subject, issuer=Issuer, id_info=IdInfo})
  when is_binary(Subject), is_binary(Issuer)->
    case maps:get(name, IdInfo, undefined) of
        undefined -> {ok, << Subject/binary, <<"@">>/binary, Issuer/binary >>};
        Other -> {ok, Other}
    end;
display_name(_) ->
    {error, not_set}.

logged_in(#user_info{subject=Subject, issuer=Issuer})
  when is_binary(Subject), is_binary(Issuer)->
    true;
logged_in(_) ->
    false.

access_token(#user_info{access_token=#{token := AccessToken}}) ->
    {ok, AccessToken};
access_token(_) ->
    {error, not_set}.


update_plugin_info(#user_info{id_info=IdInfo, id_token=IdToken,
                              token_info=TokenInfo,
                              issuer=Issuer, subject=Subject} = UserInfo) ->
    RemoveClaims = [aud, exp, nbf, iat, jti, azp, kid, aud, auth_time, at_hash,
                    c_hash],
    ReducedClaims = maps:without(RemoveClaims, maps:get(claims, IdToken, #{})),
    RemoveTokenInfo = [active, aud, sub, iss, client_id, token_type, exp, iat,
                       nbf, jti],
    ReducedTokenInfo = maps:without(RemoveTokenInfo, TokenInfo),
    IssSubUpdate = #{iss => Issuer, sub=> Subject},
    PluginInfo1 = maps:merge(IdInfo, ReducedClaims),
    PluginInfo2 = maps:merge(PluginInfo1, ReducedTokenInfo),
    PluginInfo3 = maps:merge(PluginInfo2, IssSubUpdate),
    UserInfo#user_info{plugin_info=PluginInfo3}.

maybe_to_atom(Bin) ->
    try
        binary_to_existing_atom(Bin, utf8)
    catch _:_ ->
            Bin
    end.
