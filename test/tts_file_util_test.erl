-module(tts_file_util_test).
-include_lib("eunit/include/eunit.hrl").
-include("tts.hrl").

absolute_path_test() ->
    ok = ?SETCONFIG(config_path, "/usr/local/etc/tts"),

    {ok, [[Home]]} = init:get_argument(home),
    HomeBin = list_to_binary(Home),

    Exp1 = "/usr/local/etc/tts/tts.config",
    Exp2 = "/etc/tts/tts.config",
    Exp3 = <<"/etc/tts/tts.config">>,
    Exp4_1 = <<"/usr/local/etc/tts/tts.config">>,
    Exp4_2 = <<"/etc/tts/tts.config">>,
    Exp4_3 = <<"/etc/tts/tts.config">>,
    Exp5 = << HomeBin/binary, <<"/.config/tts">>/binary >>,
    Exp6 = Home ++ "/.config/tts",
    Exp7 = "/home/tts/.config/tts.config",
    Exp8 = <<"/home/tts/.config/tts.config">>,

    Res1  = tts_file_util:to_abs("tts.config"),
    ?assertEqual(Exp1, Res1),

    Res2 = tts_file_util:to_abs("tts.config", "/etc/tts"),
    ?assertEqual(Exp2, Res2),

    Res3 = tts_file_util:to_abs("tts.config", <<"/etc/tts">>),
    ?assertEqual(Exp3, Res3),


    Res4_1 = tts_file_util:to_abs(<<"tts.config">>),
    ?assertEqual(Exp4_1, Res4_1),
    Res4_2 = tts_file_util:to_abs(<<"tts.config">>, <<"/etc/tts">>),
    ?assertEqual(Exp4_2, Res4_2),
    Res4_3 = tts_file_util:to_abs(<<"tts.config">>, "/etc/tts"),
    ?assertEqual(Exp4_3, Res4_3),

    Res5_1 = tts_file_util:to_abs(<<"~/.config/tts">>),
    ?assertEqual(Exp5, Res5_1),
    Res5_2 = tts_file_util:to_abs(<<"~/.config/tts">>, <<"/etc/tts">>),
    ?assertEqual(Exp5, Res5_2),
    Res6_1 = tts_file_util:to_abs("~/.config/tts"),
    ?assertEqual(Exp6, Res6_1),
    Res6_2 = tts_file_util:to_abs("~/.config/tts", "/etc/tts"),
    ?assertEqual(Exp6, Res6_2),

    AbsPath = "/home/tts/.config/tts.config",
    Res7_1 = tts_file_util:to_abs(AbsPath),
    ?assertEqual(Exp7, Res7_1),
    Res7_2 = tts_file_util:to_abs(AbsPath, "/etc/tts"),
    ?assertEqual(Exp7, Res7_2),


    AbsBin = <<"/home/tts/.config/tts.config">>,
    Res8_1 = tts_file_util:to_abs(AbsBin),
    ?assertEqual(Exp8, Res8_1),
    Res8_2 = tts_file_util:to_abs(AbsBin, "/etc/tts"),
    ?assertEqual(Exp8, Res8_2),

    ok = ?UNSETCONFIG(config_path),
    ok.
