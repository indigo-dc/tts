-module (watts_mail).

-include("watts.hrl").

-export([send/3]).

send(Subject, Body, Receipients) ->
    maybe_send(?CONFIG(email_enable), Subject, Body, Receipients).

maybe_send(true, Subject, Body, Receipients)
  when is_list(Receipients), length(Receipients) >= 1 ->
    Sender = ?CONFIG(email_address),
    User = ?CONFIG(email_user),
    Password = ?CONFIG(email_password),
    Server = ?CONFIG(email_server),
    Port = ?CONFIG(email_port),
    SSL = ?CONFIG(email_ssl),
    TLS = ?CONFIG(email_tls),
    Email = compose_mail(Subject, Body, Receipients, Sender),
    Filter = fun({_, Value}) ->
                     Value /= undefined
             end,
    Options = lists:filter(Filter, [{user, User}, {password, Password},
                                    {relay, Server}, {port, Port}, {ssl, SSL},
                                    {tls, TLS}, {no_mx_lookups, true}]),
    Result = gen_smtp_client:send_blocking({Sender, Receipients, Email},
                                           Options),
    handle_mail_result(Result);
maybe_send(_, Subject, _Body, []) ->
    lager:debug("Mail: no receipients for email ~p", [Subject]),
    no_receipients;
maybe_send(_, Subject, _Body, _Receipients) ->
    lager:debug("Mail: trying to send ~p but is disabled", [Subject]),
    disabled.


compose_mail(Subject, Body, Receipients, Sender) ->
    ReceipientsString = lists:flatten(lists:join(", ", Receipients)),
    Name = ?CONFIG(email_name, "WaTTS"),
    Header = [ "To: " ++ ReceipientsString,
               "Subject: " ++ Subject,
               "From: " ++ Name ++ " <" ++ Sender ++ ">",
               "Content-Type: text/plain; charset=utf-8",
               "Content-Disposition: inline",
               "Content-Transfer-Encoding: 8bit",
               "\r\n"
             ],
    lists:flatten(lists:join("\r\n", Header)) ++ Body.


handle_mail_result(Result) when is_binary(Result) ->
    lager:debug("Mail: sent with ~p", [Result]),
    ok;
handle_mail_result({error, Type, Message}) ->
    lager:error("MAIL: sending failed with ~p : ~p", [Type, Message]),
    error.
