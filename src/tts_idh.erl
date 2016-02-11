-module(tts_idh).

-export([lookup_user/1]).

lookup_user( Map) ->
    perform_some_lookup(Map).
    

perform_some_lookup(_Map) ->
   UserId = <<"joe123">>, 
   UserInfo = #{username => <<"guest">>, uid => 1111, gid => 1111},
   {ok, UserId, UserInfo}.

