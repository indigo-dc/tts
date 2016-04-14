-module(tts_file_util_test).
-include_lib("eunit/include/eunit.hrl").


absolute_path_test() ->
    ok = application:set_env(tts, config_path, "/usr/local/etc/tts"),

    Exp1 = "/usr/local/etc/tts/tts.config",
    Exp2 = "/etc/tts/tts.config",
    Exp3 = <<"/etc/tts/tts.config">>,
    Exp4 = <<"/usr/local/etc/tts/tts.config">>,

    Exp1  = tts_file_util:to_abs("tts.config"),
    Exp2 = tts_file_util:to_abs("tts.config", "/etc/tts"),
    Exp3 = tts_file_util:to_abs("tts.config", <<"/etc/tts">>),


    Exp4 = tts_file_util:to_abs(<<"tts.config">>),
    Exp3 = tts_file_util:to_abs(<<"tts.config">>, <<"/etc/tts">>),
    Exp3 = tts_file_util:to_abs(<<"tts.config">>, "/etc/tts"),

    AbsPath = "/home/tts/.config/tts.config",
    AbsPath = tts_file_util:to_abs(AbsPath),
    AbsPath = tts_file_util:to_abs(AbsPath, "/etc/tts"),

    AbsBin = <<"/home/tts/.config/tts.config">>,
    AbsBin = tts_file_util:to_abs(AbsBin),
    AbsBin = tts_file_util:to_abs(AbsBin, "/etc/tts"),

    ok = application:unset_env(tts, config_path),
    ok.
