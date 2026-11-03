-module(hikyaku_adapter_smtp).
-behaviour(hikyaku_adapter).

-include("hikyaku.hrl").

-export([deliver/2, build_mimemail/1, build_smtp_opts/1]).

-spec deliver(#hikyaku_email{}, map()) -> {ok, map()} | {error, term()}.
deliver(Email, Config) ->
    MimeMsg = build_mimemail(Email),
    SmtpOpts = build_smtp_opts(Config),
    #hikyaku_email{from = #hikyaku_address{address = FromAddr}} = Email,
    AllRecipients = collect_recipients(Email),
    EncodedMsg = mimemail:encode(MimeMsg),
    case
        gen_smtp_client:send_blocking(
            {binary_to_list(FromAddr), [binary_to_list(R) || R <- AllRecipients], EncodedMsg},
            SmtpOpts
        )
    of
        Receipt when is_binary(Receipt) ->
            {ok, #{status => sent, receipt => Receipt}};
        {error, Type, Msg} ->
            {error, #{type => Type, message => Msg}};
        {error, Reason} ->
            {error, Reason}
    end.

%%% Internal

collect_recipients(#hikyaku_email{to = To, cc = Cc, bcc = Bcc}) ->
    [A#hikyaku_address.address || A <- To ++ Cc ++ Bcc].

build_smtp_opts(Config) ->
    Relay = binary_to_list(maps:get(relay, Config)),
    Port = maps:get(port, Config, 587),
    Username = maps:get(username, Config, undefined),
    Password = maps:get(password, Config, undefined),
    Ssl = maps:get(ssl, Config, false),
    Opts0 = [
        {relay, Relay},
        {port, Port}
    ],
    Opts1 =
        case Username of
            undefined ->
                Opts0;
            _ ->
                Opts0 ++
                    [{username, binary_to_list(Username)}, {password, binary_to_list(Password)}]
        end,
    %% gen_smtp hands `tls_options` only to STARTTLS; an implicit TLS (ssl)
    %% connection takes its ssl options from `sockopts`.
    Opts2 =
        case Ssl of
            true -> Opts1 ++ [{ssl, true}, {sockopts, tls_options(Relay, Config)}];
            false -> Opts1
        end,
    Tls = maps:get(tls, Config, default_tls(Ssl, Username)),
    Opts3 =
        case Tls of
            always -> Opts2 ++ [{tls, always}];
            never -> Opts2 ++ [{tls, never}];
            if_available -> Opts2 ++ [{tls, if_available}]
        end,
    Opts4 =
        case Tls of
            never -> Opts3;
            _ -> Opts3 ++ [{tls_options, tls_options(Relay, Config)}]
        end,
    case maps:get(no_mx_lookups, Config, false) of
        true -> Opts4 ++ [{no_mx_lookups, true}];
        false -> Opts4
    end.

%% With credentials configured, `if_available` would let an on-path attacker
%% strip STARTTLS from the EHLO reply and read the AUTH exchange in clear.
default_tls(false, Username) when Username =/= undefined -> always;
default_tls(_Ssl, _Username) -> if_available.

%% gen_smtp may dial an MX host or a resolved IP address rather than the relay
%% name, and a STARTTLS upgrade has no host name at all, so the relay name is
%% pinned as SNI and as the reference identity for certificate verification.
tls_options(Relay, Config) ->
    TlsOpts = maps:get(tls_options, Config, default_tls_options()),
    case proplists:is_defined(server_name_indication, TlsOpts) orelse is_ip_address(Relay) of
        true -> TlsOpts;
        false -> [{server_name_indication, Relay} | TlsOpts]
    end.

is_ip_address(Host) ->
    element(1, inet:parse_address(Host)) =:= ok.

default_tls_options() ->
    [
        {verify, verify_peer},
        {cacerts, public_key:cacerts_get()},
        {depth, 3},
        {versions, ['tlsv1.2', 'tlsv1.3']},
        {customize_hostname_check, [
            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
        ]}
    ].

build_mimemail(Email) ->
    #hikyaku_email{
        subject = Subject,
        from = From,
        to = To,
        cc = Cc,
        text_body = Text,
        html_body = Html,
        reply_to = ReplyTo,
        headers = CustomHeaders,
        attachments = Attachments
    } = Email,
    Headers0 = [
        {<<"From">>, format_address(From)},
        {<<"To">>, format_address_list(To)},
        {<<"Subject">>, Subject}
    ],
    Headers1 =
        case Cc of
            [] -> Headers0;
            _ -> Headers0 ++ [{<<"Cc">>, format_address_list(Cc)}]
        end,
    Headers2 =
        case ReplyTo of
            undefined -> Headers1;
            _ -> Headers1 ++ [{<<"Reply-To">>, format_address(ReplyTo)}]
        end,
    Headers3 = Headers2 ++ maps:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], CustomHeaders),
    {Type, SubType, BodyHeaders, _Params, SubParts} = build_body(Text, Html, Attachments),
    {Type, SubType, Headers3 ++ BodyHeaders, #{}, SubParts}.

build_body(Text, Html, Attachments) when Attachments =/= [] ->
    ContentParts = content_parts(Text, Html),
    AttParts = [build_attachment_part(A) || A <- Attachments],
    {<<"multipart">>, <<"mixed">>, [], #{}, ContentParts ++ AttParts};
build_body(Text, Html, []) ->
    case {Text, Html} of
        {undefined, undefined} ->
            {<<"text">>, <<"plain">>, [], #{}, <<>>};
        {T, undefined} when is_binary(T) ->
            {<<"text">>, <<"plain">>, [], #{}, T};
        {undefined, H} when is_binary(H) ->
            {<<"text">>, <<"html">>, [], #{}, H};
        {T, H} when is_binary(T), is_binary(H) ->
            {<<"multipart">>, <<"alternative">>, [], #{}, [
                {<<"text">>, <<"plain">>, [], #{}, T},
                {<<"text">>, <<"html">>, [], #{}, H}
            ]}
    end.

content_parts(undefined, undefined) ->
    [];
content_parts(T, undefined) when is_binary(T) ->
    [{<<"text">>, <<"plain">>, [], #{}, T}];
content_parts(undefined, H) when is_binary(H) ->
    [{<<"text">>, <<"html">>, [], #{}, H}];
content_parts(T, H) when is_binary(T), is_binary(H) ->
    [
        {<<"multipart">>, <<"alternative">>, [], #{}, [
            {<<"text">>, <<"plain">>, [], #{}, T},
            {<<"text">>, <<"html">>, [], #{}, H}
        ]}
    ].

build_attachment_part(#hikyaku_attachment{
    filename = Filename,
    content_type = ContentType,
    data = Data0,
    path = Path,
    disposition = Disposition,
    cid = Cid
}) ->
    Data =
        case Data0 of
            undefined when is_binary(Path) -> read_file(Path);
            Bin when is_binary(Bin) -> Bin
        end,
    [Type, SubType] = binary:split(ContentType, <<"/">>),
    DispBin =
        case Disposition of
            inline -> <<"inline">>;
            attachment -> <<"attachment">>
        end,
    Headers =
        case Cid of
            undefined -> [];
            _ -> [{<<"Content-ID">>, <<"<", Cid/binary, ">">>}]
        end,
    Params = #{
        disposition => DispBin,
        disposition_params => [{~"filename", Filename}],
        transfer_encoding => ~"base64"
    },
    {Type, SubType, Headers, Params, Data}.

read_file(Path) ->
    {ok, Data} = file:read_file(Path),
    Data.

format_address(#hikyaku_address{name = undefined, address = Addr}) ->
    Addr;
format_address(#hikyaku_address{name = Name, address = Addr}) ->
    <<Name/binary, " <", Addr/binary, ">">>.

format_address_list(Addrs) ->
    Formatted = [format_address(A) || A <- Addrs],
    iolist_to_binary(lists:join(<<", ">>, Formatted)).
