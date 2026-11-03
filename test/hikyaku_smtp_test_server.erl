-module(hikyaku_smtp_test_server).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/src/inet_dns.hrl").

-export([pki/0, pki/1, start/2, stop/1, start_dns/0, stop_dns/1]).

pki() ->
    pki(0).

pki(Intermediates) ->
    San = #'Extension'{
        extnID = ?'id-ce-subjectAltName',
        extnValue = [{dNSName, "localhost"}],
        critical = false
    },
    Cert = [{key, {namedCurve, ?secp256r1}}, {digest, sha256}],
    #{server_config := Server, client_config := Client} = public_key:pkix_test_data(#{
        server_chain => #{
            root => Cert,
            intermediates => [Cert || _ <- lists:seq(1, Intermediates)],
            peer => [{extensions, [San]} | Cert]
        },
        client_chain => #{root => Cert, intermediates => [], peer => Cert}
    }),
    #{server => Server, cacerts => proplists:get_value(cacerts, Client)}.

start(Mode, SslOpts) ->
    Parent = self(),
    Pid = spawn_link(fun() ->
        {ok, Listen} = gen_tcp:listen(0, [
            binary, {packet, line}, {active, false}, {reuseaddr, true}, {ip, {127, 0, 0, 1}}
        ]),
        {ok, Port} = inet:port(Listen),
        Parent ! {self(), port, Port},
        accept_loop(Listen, Mode, SslOpts, Parent)
    end),
    receive
        {Pid, port, Port} -> {Pid, Port}
    after 5000 -> error(smtp_test_server_timeout)
    end.

stop(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    ok.

accept_loop(Listen, Mode, SslOpts, Parent) ->
    {ok, Sock} = gen_tcp:accept(Listen),
    session(Sock, Mode, SslOpts, Parent),
    accept_loop(Listen, Mode, SslOpts, Parent).

session(Sock, ssl, SslOpts, Parent) ->
    case ssl:handshake(Sock, SslOpts, 5000) of
        {ok, Tls} ->
            greet({ssl, Tls}, ssl, SslOpts, Parent);
        {error, Reason} ->
            Parent ! {smtp_test_server, {tls_failed, Reason}},
            gen_tcp:close(Sock)
    end;
session(Sock, Mode, SslOpts, Parent) ->
    greet({tcp, Sock}, Mode, SslOpts, Parent).

greet(Conn, Mode, SslOpts, Parent) ->
    send(Conn, ~"220 localhost ESMTP\r\n"),
    command_loop(Conn, Mode, SslOpts, Parent).

command_loop(Conn, Mode, SslOpts, Parent) ->
    case recv(Conn) of
        {ok, Line} ->
            handle(command(Line), Line, Conn, Mode, SslOpts, Parent);
        {error, _} ->
            close(Conn)
    end.

command(<<Cmd:4/binary, _/binary>>) -> string:uppercase(Cmd);
command(Line) -> string:uppercase(string:trim(Line)).

handle(~"EHLO", _Line, Conn, Mode, SslOpts, Parent) ->
    StartTls =
        case {Mode, Conn} of
            {starttls, {tcp, _}} -> [~"250-STARTTLS\r\n"];
            _ -> []
        end,
    send(Conn, [~"250-localhost\r\n", StartTls, ~"250 AUTH PLAIN\r\n"]),
    command_loop(Conn, Mode, SslOpts, Parent);
handle(~"STAR", _Line, {tcp, Sock} = Conn, Mode, SslOpts, Parent) ->
    send(Conn, ~"220 ready\r\n"),
    case ssl:handshake(Sock, SslOpts, 5000) of
        {ok, Tls} ->
            command_loop({ssl, Tls}, Mode, SslOpts, Parent);
        {error, Reason} ->
            Parent ! {smtp_test_server, {tls_failed, Reason}},
            gen_tcp:close(Sock)
    end;
handle(~"AUTH", _Line, Conn, Mode, SslOpts, Parent) ->
    Parent ! {smtp_test_server, {auth, transport(Conn)}},
    send(Conn, ~"235 ok\r\n"),
    command_loop(Conn, Mode, SslOpts, Parent);
handle(~"DATA", _Line, Conn, Mode, SslOpts, Parent) ->
    send(Conn, ~"354 go\r\n"),
    Data = read_data(Conn, <<>>),
    Parent ! {smtp_test_server, {data, transport(Conn), Data}},
    send(Conn, ~"250 queued\r\n"),
    command_loop(Conn, Mode, SslOpts, Parent);
handle(~"QUIT", _Line, Conn, _Mode, _SslOpts, _Parent) ->
    send(Conn, ~"221 bye\r\n"),
    close(Conn);
handle(_Other, _Line, Conn, Mode, SslOpts, Parent) ->
    send(Conn, ~"250 ok\r\n"),
    command_loop(Conn, Mode, SslOpts, Parent).

read_data(Conn, Acc) ->
    case recv(Conn) of
        {ok, ~".\r\n"} -> Acc;
        {ok, Line} when is_binary(Line) -> read_data(Conn, <<Acc/binary, Line/binary>>);
        _ -> Acc
    end.

transport({tcp, _}) -> tcp;
transport({ssl, _}) -> ssl.

send({tcp, Sock}, Data) -> gen_tcp:send(Sock, Data);
send({ssl, Sock}, Data) -> ssl:send(Sock, Data).

recv({tcp, Sock}) -> gen_tcp:recv(Sock, 0, 5000);
recv({ssl, Sock}) -> ssl:recv(Sock, 0, 5000).

close({tcp, Sock}) -> gen_tcp:close(Sock);
close({ssl, Sock}) -> ssl:close(Sock).

%% A resolver that answers every MX query with `localhost`, standing in for
%% a spoofed DNS reply. Returns the saved resolver state for stop_dns/1.
start_dns() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}, {active, false}]),
    {ok, Port} = inet:port(Sock),
    Pid = spawn(fun() ->
        receive
            go -> dns_loop(Sock)
        end
    end),
    ok = gen_udp:controlling_process(Sock, Pid),
    Pid ! go,
    Keys = [resolv_conf, nameservers, alt_nameservers, search],
    Saved = [{K, inet_db:res_option(K)} || K <- Keys],
    ok = inet_db:res_option(resolv_conf, ""),
    ok = inet_db:res_option(nameservers, [{{127, 0, 0, 1}, Port}]),
    ok = inet_db:res_option(alt_nameservers, []),
    ok = inet_db:res_option(search, []),
    {Pid, Saved}.

stop_dns({Pid, Saved}) ->
    [ok = inet_db:res_option(K, V) || {K, V} <- Saved],
    exit(Pid, kill),
    ok.

dns_loop(Sock) ->
    case gen_udp:recv(Sock, 0) of
        {ok, {{_, _, _, _} = Ip, Port, Packet}} ->
            {ok,
                #dns_rec{header = Header, qdlist = [#dns_query{domain = Domain, type = Type}]} =
                    Query} =
                inet_dns:decode(Packet),
            Answers =
                case Type of
                    mx ->
                        [
                            #dns_rr{
                                domain = Domain,
                                type = mx,
                                class = in,
                                ttl = 60,
                                data = {10, "localhost"}
                            }
                        ];
                    _ ->
                        []
                end,
            Reply = Query#dns_rec{
                header = Header#dns_header{qr = true, aa = true, ra = true}, anlist = Answers
            },
            ok = gen_udp:send(Sock, Ip, Port, inet_dns:encode(Reply)),
            dns_loop(Sock);
        {error, _} ->
            ok
    end.
