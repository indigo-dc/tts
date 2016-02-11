-module(tts_debug). 

-export([
         start/0,
         add/1,
         stop/0,
         ui/0
        ]).

ui() ->
    start(),
    add([tts_http,tts_session,tts_user,tts_session_mgr,tts_user_mgr]),
    ok.

start() ->
   dbg:start(),
   dbg:tracer().


add([]) ->
    dbg:p(all,call);
add([H|T]) ->
    dbg:tpl(H,[]),
    add(T).


stop() ->
    dbg:stop_clear().
