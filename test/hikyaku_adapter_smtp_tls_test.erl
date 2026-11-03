-module(hikyaku_adapter_smtp_tls_test).
-include_lib("eunit/include/eunit.hrl").

tls_test_() ->
    {setup,
        fun() ->
            {ok, _} = application:ensure_all_started(ssl),
            hikyaku_smtp_test_server:pki()
        end,
        fun(Pki) ->
            [
                {"STARTTLS verifies the relay hostname", fun() -> starttls_trusted(Pki) end},
                {"STARTTLS rejects an untrusted relay by default", fun() ->
                    starttls_untrusted(Pki)
                end},
                {"credentials are never sent without TLS by default", fun() ->
                    credentials_need_tls(Pki)
                end},
                {"implicit TLS honours tls_options", fun() -> ssl_trusted(Pki) end},
                {"implicit TLS rejects an untrusted relay by default", fun() ->
                    ssl_untrusted(Pki)
                end},
                {"caller tls_options accept a chain with an intermediate", fun intermediate_chain/0}
            ]
        end}.

starttls_trusted(#{cacerts := CaCerts} = Pki) ->
    with_server(starttls, Pki, fun(Port) ->
        Config = config(Port, #{
            tls => always,
            username => ~"user",
            password => ~"secret",
            tls_options => [{verify, verify_peer}, {cacerts, CaCerts}]
        }),
        ?assertMatch({ok, #{status := sent}}, hikyaku_adapter_smtp:deliver(email(), Config)),
        ?assertEqual({auth, ssl}, next_event()),
        ?assertMatch({data, ssl, _}, next_event())
    end).

starttls_untrusted(Pki) ->
    with_server(starttls, Pki, fun(Port) ->
        Config = config(Port, #{tls => always, username => ~"user", password => ~"secret"}),
        ?assertMatch({error, _}, hikyaku_adapter_smtp:deliver(email(), Config)),
        ?assertNot(auth_seen())
    end).

credentials_need_tls(Pki) ->
    with_server(plain, Pki, fun(Port) ->
        Config = config(Port, #{username => ~"user", password => ~"secret"}),
        ?assertMatch({error, _}, hikyaku_adapter_smtp:deliver(email(), Config)),
        ?assertNot(auth_seen())
    end).

ssl_trusted(#{cacerts := CaCerts} = Pki) ->
    with_server(ssl, Pki, fun(Port) ->
        Config = config(Port, #{
            ssl => true,
            username => ~"user",
            password => ~"secret",
            tls_options => [{verify, verify_peer}, {cacerts, CaCerts}]
        }),
        ?assertMatch({ok, #{status := sent}}, hikyaku_adapter_smtp:deliver(email(), Config)),
        ?assertEqual({auth, ssl}, next_event())
    end).

ssl_untrusted(Pki) ->
    with_server(ssl, Pki, fun(Port) ->
        Config = config(Port, #{ssl => true, username => ~"user", password => ~"secret"}),
        ?assertMatch({error, _}, hikyaku_adapter_smtp:deliver(email(), Config)),
        ?assertNot(auth_seen())
    end).

ip_relay_mx_test_() ->
    {setup,
        fun() ->
            {ok, _} = application:ensure_all_started(ssl),
            hikyaku_smtp_test_server:start_dns()
        end,
        fun hikyaku_smtp_test_server:stop_dns/1, fun(Dns) ->
            [
                {"an IP relay is dialled directly, never through an MX lookup", fun() ->
                    ip_relay_ignores_mx(Dns)
                end}
            ]
        end}.

ip_relay_ignores_mx(Dns) ->
    #{cacerts := CaCerts} = Pki = hikyaku_smtp_test_server:pki(),
    with_server(ssl, Pki, fun(Port) ->
        Opts = hikyaku_adapter_smtp:build_smtp_opts(#{
            relay => ~"127.0.0.2",
            port => Port,
            ssl => true,
            username => ~"user",
            password => ~"secret",
            tls_options => [{verify, verify_peer}, {cacerts, CaCerts}]
        }),
        Result = gen_smtp_client:send_blocking(
            {"alice@example.com", ["bob@example.com"], ~"Subject: hi\r\n\r\nhello"},
            [{timeout, 2000}, {retries, 0} | Opts]
        ),
        ?assertNotMatch(Receipt when is_binary(Receipt), Result),
        ?assertEqual([], [Q || {_, mx} = Q <- hikyaku_smtp_test_server:dns_queries(Dns)]),
        ?assertNot(auth_seen())
    end).

intermediate_chain() ->
    #{cacerts := CaCerts} = Pki = hikyaku_smtp_test_server:pki(1),
    with_server(ssl, Pki, fun(Port) ->
        Config = config(Port, #{
            ssl => true,
            username => ~"user",
            password => ~"secret",
            tls_options => [{verify, verify_peer}, {cacerts, CaCerts}]
        }),
        ?assertMatch({ok, #{status := sent}}, hikyaku_adapter_smtp:deliver(email(), Config)),
        ?assertEqual({auth, ssl}, next_event())
    end).

%%% Helpers

with_server(Mode, #{server := ServerOpts}, Test) ->
    SslOpts = [binary, {packet, line}, {active, false} | ServerOpts],
    {Pid, Port} = hikyaku_smtp_test_server:start(Mode, SslOpts),
    try
        Test(Port)
    after
        hikyaku_smtp_test_server:stop(Pid),
        flush()
    end.

config(Port, Extra) ->
    maps:merge(
        #{relay => ~"localhost", port => Port, no_mx_lookups => true},
        Extra
    ).

email() ->
    hikyaku_email:subject(
        hikyaku_email:text_body(
            hikyaku_email:to(
                hikyaku_email:from(hikyaku_email:new(), ~"alice@example.com"),
                ~"bob@example.com"
            ),
            ~"hello"
        ),
        ~"Hi"
    ).

next_event() ->
    receive
        {smtp_test_server, {tls_failed, _}} -> next_event();
        {smtp_test_server, Event} -> Event
    after 5000 -> error(no_smtp_event)
    end.

auth_seen() ->
    receive
        {smtp_test_server, {auth, _}} -> true;
        {smtp_test_server, _} -> auth_seen()
    after 200 -> false
    end.

flush() ->
    receive
        {smtp_test_server, _} -> flush()
    after 0 -> ok
    end.
