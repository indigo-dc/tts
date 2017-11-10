-module(watts_mail_test).
-include_lib("eunit/include/eunit.hrl").
-include("watts.hrl").

disabled_test() ->
    ?SETCONFIG(email_enabled, false),
    Subject = "test",
    Body = "test",
    Receipients = ["some@example.com"],
    disabled = watts_mail:send(Subject, Body, Receipients),
    ?UNSETCONFIG(email_enabled),
    disabled = watts_mail:send(Subject, Body, Receipients).

no_receipients_test() ->
    ?SETCONFIG(email_enabled, true),
    Subject = "test",
    Body = "test",
    no_receipients = watts_mail:send(Subject, Body, []),
    no_receipients = watts_mail:send(Subject, Body, <<>>).


send_test() ->
    {ok, Meck} = start_meck(),
    try
        ?SETCONFIG(email_enabled, true),
        Subject = "test",
        Body = "test",
        Receipients = ["some@example.com"],
        ok = watts_mail:send(Subject, Body, Receipients)
    after
        ok = stop_meck(Meck)
    end,
    ok.

own_address_test() ->
    {ok, Meck} = start_meck(),
    try
        ?SETCONFIG(email_enabled, true),
        ?SETCONFIG(email_address, "error1@test.com"),
        Subject = "address",
        Body = "test",
        Receipients = ["some@example.com"],
        error = watts_mail:send(Subject, Body, Receipients)
    after
        ok = stop_meck(Meck)
    end,
    ok.

error2_test() ->
    {ok, Meck} = start_meck(),
    try
        ?SETCONFIG(email_enabled, true),
        ?SETCONFIG(email_address, "error2@test.com"),
        Subject = "address",
        Body = "test",
        Receipients = ["some@example.com"],
        error = watts_mail:send(Subject, Body, Receipients)
    after
        ok = stop_meck(Meck)
    end,
    ok.

start_meck() ->
    ?SETCONFIG(hostname, "localhost"),
    MeckModules = [gen_smtp_client],
    Send = fun({Sender, _Receipients, _Email}, _Options) ->
                   case Sender of
                       "error1@test.com" ->
                          {error, used_own_address};
                       "error2@test.com" ->
                           {error, bad, "this it just a test"};
                       S when is_list(S) ->
                           <<"some valid binary result">>
                   end
            end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(gen_smtp_client, send_blocking, Send),
    {ok, {MeckModules}}.


stop_meck({MeckModules}) ->
    ok = test_util:meck_done(MeckModules),
    ?UNSETCONFIG(email_enabled),
    ?UNSETCONFIG(hostname),
    ok.
