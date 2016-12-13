-module(tts_userinfo_test).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    {ok, Info} = tts_userinfo:new(),
    {error, not_found} = tts_userinfo:return({key, id}, Info),
    {ok, #{}} = tts_userinfo:return(plugin_info, Info),
    {ok, undefined} = tts_userinfo:return(issuer, Info),
    {ok, undefined} = tts_userinfo:return(subject, Info),
    {ok, undefined, undefined} = tts_userinfo:return(issuer_subject, Info),
    {error, not_set} = tts_userinfo:return(id, Info),
    {error, not_set} = tts_userinfo:return(access_token, Info),
    {error, not_set} = tts_userinfo:return(display_name, Info),
    false = tts_userinfo:return(logged_in, Info),
    ok.


update_iss_sub_test() ->
    Issuer = <<"issuer">>,
    Iss = <<"iss">>,
    Sub = <<"sub">>,
    {ok, Info} = tts_userinfo:new(),
    {ok, undefined, undefined} = tts_userinfo:return(issuer_subject, Info),
    {error, _} = tts_userinfo:update_iss_sub(Issuer, undefined, Info),
    {ok, Info2} = tts_userinfo:update_iss_sub(Issuer, Sub, Info),
    {ok, Issuer, Sub} = tts_userinfo:return(issuer_subject, Info2),
    {ok, Info2} = tts_userinfo:update_iss_sub(Issuer, Sub, Info2),
    {ok, Issuer, Sub} = tts_userinfo:return(issuer_subject, Info2),
    {error, _} = tts_userinfo:update_iss_sub(Iss, <<"su">>, Info2),
    {ok, Info3} = tts_userinfo:update_iss_sub(Iss, Sub, Info),
    {ok, Iss, Sub} = tts_userinfo:return(issuer_subject, Info3),
    ok.

update_id_token_test() ->
    Issuer = <<"https://iam.it">>,
    Sub = <<"sub">>,
    Acr = <<"https://egi.eu/LoA#Substantial">>,
    Groups2 = <<"good,bad,bad-good,not_good ">>,
    {ok, UserInfo0} = tts_userinfo:new(),
    IdToken = #{claims => #{sub => Sub,
                            iss => Issuer,
                            groups => [<<"Developer">>, <<"User">>],
                            acr => Acr,
                            groups2 => Groups2,
                            groups3 => <<"good,bad,tbad-good,not_good ">>
                           }},
    IdToken2 = #{claims => #{sub => Sub, iss => <<"iss">> }},
    IdToken3 = #{claims => #{sub => <<"su">>, iss => Issuer }},
    {ok, UserInfo1} = tts_userinfo:update_id_token(IdToken
                                                 , UserInfo0),
    {error, _} = tts_userinfo:update_id_token(IdToken2, UserInfo1),
    {error, _} = tts_userinfo:update_id_token(IdToken3, UserInfo1),
    {ok, Issuer, Sub} = tts_userinfo:return(issuer_subject, UserInfo1),
    {ok, Issuer} = tts_userinfo:return({key, <<"iss">>}, UserInfo1),
    {ok, Sub} = tts_userinfo:return({key, <<"sub">>}, UserInfo1),
    {ok, Acr} = tts_userinfo:return({key, <<"acr">>}, UserInfo1),
    {ok, Groups2} = tts_userinfo:return({key, <<"groups2">>}, UserInfo1),
    {ok, _} = tts_userinfo:return(display_name, UserInfo1),
    {ok, _} = tts_userinfo:return(id, UserInfo1),
    true = tts_userinfo:return(logged_in, UserInfo1),
    ok.

update_access_token_test() ->
    {ok, Info} = tts_userinfo:new(),
    AtData = <<"simple test access token">>,
    AccessToken = #{ token => AtData},
    BadAT = #{token => <<"simpler test access token">>},
    AtHash = <<"qnOSv0PoaVdwcWU6IMyypQ">>,
    IdToken = #{claims => #{at_hash => AtHash,
                            iss => <<"iss">>,
                            sub => <<"sub">>}},
    {ok, Info2} = tts_userinfo:update_id_token(IdToken, Info),
    {error, not_set} = tts_userinfo:return(access_token, Info2),
    {ok, _} = tts_userinfo:update_access_token(AccessToken, Info),
    {ok, _} = tts_userinfo:update_access_token(BadAT, Info),
    {ok, Info3} = tts_userinfo:update_access_token(AccessToken, Info2),
    {error, _} = tts_userinfo:update_access_token(BadAT, Info2),
    {error, _} = tts_userinfo:update_access_token(BadAT, Info3),
    {ok, AtData} = tts_userinfo:return(access_token, Info3),

    ok.

update_id_info_test() ->
    {ok, Info} = tts_userinfo:new(),
    Joe = <<"joe">>,
    Iss = <<"iss">>,
    Sub = <<"sub">>,
    IdInfo1 = #{sub => Sub, name => Joe},
    IdInfo2 = #{sub => <<"s">>, name => Joe},
    {ok, Info1} = tts_userinfo:update_iss_sub(Iss, Sub, Info),
    {ok, Info2} = tts_userinfo:update_id_info(IdInfo1, Info1),
    {error, _} = tts_userinfo:update_id_info(IdInfo2, Info1),

    {ok, Joe} = tts_userinfo:return(display_name, Info2),

    {ok, Info3} = tts_userinfo:update_id_info(IdInfo1, Info),
    {ok, Info4} = tts_userinfo:update_id_info(IdInfo2, Info),
    {error, not_set} = tts_userinfo:return(display_name, Info3),
    {error, not_set} = tts_userinfo:return(display_name, Info4),

    ok.
