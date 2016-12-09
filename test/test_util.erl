-module(test_util).

-export([wait_for_process_to_die/2]).
-export([wait_for_true/2]).
-export([session_running/1]).
-export([meck_new/1]).
-export([meck_done/1]).


session_running(Pid) ->
    case process_info(Pid) of
        undefined ->
            false;
        _ ->
            true
    end.


wait_for_process_to_die(Pid, Iterations) ->
    PidWait = fun() ->
                      not session_running(Pid)
              end,
        case wait_for_true(PidWait, Iterations) of
            ok -> ok;
            _ -> still_alive
        end.

wait_for_true(_Fun, 0) ->
    failed;
wait_for_true(Fun, Iterations) ->
    case Fun() of
        true ->
            ok;
        _ ->
            timer:sleep(10),
            wait_for_true(Fun, Iterations-1)
    end.

meck_new([]) ->
    ok;
meck_new([H | T]) ->
    meck:new(H),
    meck_new(T).


meck_done([]) ->
    ok;
meck_done([H | T]) ->
    true = meck:validate(H),
    ok = meck:unload(H),
    meck_done(T).
