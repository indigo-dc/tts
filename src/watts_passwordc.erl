-module(watts_passwordc).

-export([get_password/1]).


%% @doc retrieve passwords from the passwordd deamon
-spec get_password(binary()) -> {ok, binary()} | {error, atom()}.
get_password(Key) when is_binary(Key) ->
    {ok, Sock} = gen_tcp:connect("localhost", 6969, [binary, {packet, 0},
                                                     {active, false}]),
    Request = jsone:encode(#{action => get, key => Key}),
    ok = gen_tcp:send(Sock, Request),
    {ok, Response} = gen_tcp:recv(Sock, 0),
    ok = gen_tcp:close(Sock),
    case jsone:try_decode(Response, [{object_format, map},
                                     {keys, attempt_atom}]) of

        {ok, #{result := <<"ok">>, value := Passwd}, _} ->
            {ok, Passwd};
        {ok, #{result := <<"error">>}, _} ->
                {error, from_passwordd};
             {error, _} ->
                {error, bad_json}
        end.
