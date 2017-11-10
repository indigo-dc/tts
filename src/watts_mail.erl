%% @doc a simple mail sending implementation using gen_smtp
-module (watts_mail).

-include("watts.hrl").

-export([send/3]).

%% @doc send a mail (if enabled), given subject, body and receipients
-spec send(string(), string(), [string()]) -> atom().
send(Subject, Body, Receipients) ->
    maybe_send(?CONFIG(mail_enabled), Subject, Body, Receipients).


%% @doc send a mail if configured, given subject, body and receipients
-spec maybe_send(boolean(), string(), string(), [string()]) -> atom().
maybe_send(true, Subject, Body, Receipients)
  when is_list(Receipients), length(Receipients) >= 1 ->
    User = ?CONFIG(email_user),
    Password = ?CONFIG(email_password),
    Relay = ?CONFIG(email_relay),
    NoMxLookups = ?CONFIG(email_no_mx_lookups),
    Port = ?CONFIG(email_port),
    SSL = ?CONFIG(email_ssl),
    TLS = ?CONFIG(email_tls, always),
    Sender = sender(),
    Email = compose_mail(Subject, Body, Receipients, Sender),
    Filter = fun({_, Value}) ->
                     Value /= undefined
             end,
    Options = lists:filter(Filter, [{user, User}, {password, Password},
                                    {relay, Relay}, {port, Port}, {ssl, SSL},
                                    {tls, TLS}, {no_mx_lookups, NoMxLookups}]),
    Result = gen_smtp_client:send_blocking({Sender, Receipients, Email},
                                           Options),
    handle_mail_result(Result);
maybe_send(true, Subject, _Body, []) ->
    lager:debug("Mail: no receipients for email ~p", [Subject]),
    no_receipients;
maybe_send(_, Subject, _Body, _Receipients) ->
    lager:debug("Mail: trying to send ~p but is disabled", [Subject]),
    disabled.


%% @doc create the whole mail with headers
-spec compose_mail(string(), string(), [string()], string()) -> string().
compose_mail(Subject, Body, Receipients, Sender) ->
    ReceipientsString = lists:flatten(watts_utils:lists_join(", ", Receipients)),
    Name = ?CONFIG(email_name, "WaTTS"),
    Header = [ "To: " ++ ReceipientsString,
               "Subject: " ++ Subject,
               "From: " ++ Name ++ " <" ++ Sender ++ ">",
               "Content-Type: text/plain; charset=utf-8",
               "Content-Disposition: inline",
               "Content-Transfer-Encoding: 8bit",
               "\r\n"
             ],
    lists:flatten(watts_utils:lists_join("\r\n", Header)) ++ Body.


%% @doc handle mail results and return either ok or error, also log errors
-spec handle_mail_result(binary() | {error, any(), any()}) -> ok | error.
handle_mail_result(Result) when is_binary(Result) ->
    lager:debug("Mail: sent with ~p", [Result]),
    ok;
handle_mail_result({error, Reason}) ->
    lager:error("MAIL: sending failed with error ~p", [Reason]),
    error;
handle_mail_result({error, Type, Message}) ->
    lager:error("MAIL: sending failed with ~p : ~p", [Type, Message]),
    error.

%% @doc get the sender
-spec sender() -> list().
sender() ->
    default_or_given_sender(?CONFIG(email_address)).

%% @doc decide between given sender and default
-spec default_or_given_sender(Sender) -> list()
   when
      Sender :: list() | any().
default_or_given_sender(Sender) when is_list(Sender) ->
    Sender;
default_or_given_sender(_) ->
    "watts@" ++ ?CONFIG(hostname).
