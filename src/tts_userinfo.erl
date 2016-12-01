-module(tts_userinfo).

-export([
         new/0,
         update_iss_sub/3,
         update_id_token/2,
         update_access_token/2,
         update_id_info/2,
         return/2
        ]).

-record(user_info, {
          issuer = undefined,
          subject = undefined,
          id_token = #{},
          id_info = #{},
          access_token = #{},
          plugin_info = #{}
         }).

new() ->
    {ok, #user_info{}}.


update_iss_sub(Issuer, Subject, Info) ->
    {ok, Info#user_info{issuer=Issuer, subject=Subject}}.

update_id_token(IdToken, #user_info{issuer=Issuer, subject=Subject}=Info) ->
    Claims = maps:get(claims, IdToken, #{}),
    Iss = maps:get(iss, Claims),
    Sub = maps:get(sub, Claims),
    case {Issuer, Subject} of
        {Iss, Sub} ->
            {ok, update_plugin_info(Info#user_info{id_token=IdToken})};
        {undefined, undefined} ->
            {ok, update_plugin_info(Info#user_info{id_token=IdToken, issuer=Iss,
                                                   subject=Sub})};
        _ ->
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

update_id_info(IdInfo, #user_info{subject=Subject}=Info) ->
    Sub = maps:get(sub, IdInfo),
    case Sub of
        Subject ->
            {ok, update_plugin_info(Info#user_info{id_info=IdInfo})};
        _ ->
            {error, not_match}
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


update_plugin_info(#user_info{id_info=IdInfo, id_token=IdToken} = UserInfo) ->
    RemoveClaims = [aud, exp, nbf, iat, jti, azp, kid, aud, auth_time, at_hash,
                    c_hash],
    ReducedClaims = maps:without(RemoveClaims, maps:get(claims, IdToken, #{})),
    PluginInfo = maps:merge(IdInfo, ReducedClaims),
    UserInfo#user_info{plugin_info=PluginInfo}.

maybe_to_atom(Bin) ->
    try
        binary_to_existing_atom(Bin, utf8)
    catch _:_ ->
            Bin
    end.
