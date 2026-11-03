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
    Tls = maps:get(tls, Config, default_tls(Ssl, Username, Relay)),
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
    case no_mx_lookups(Relay, Config) of
        true -> Opts4 ++ [{no_mx_lookups, true}];
        false -> Opts4
    end.

%% `if_available` lets an on-path attacker strip STARTTLS from the EHLO reply
%% and read the AUTH exchange and the message in clear, so it is only the
%% default for an unauthenticated loopback relay.
default_tls(true, _Username, _Relay) ->
    if_available;
default_tls(false, undefined, Relay) ->
    case is_loopback(Relay) of
        true -> if_available;
        false -> always
    end;
default_tls(false, _Username, _Relay) ->
    always.

%% Without a name to verify against, the certificate check cannot tell the
%% relay from whatever host a spoofed MX answer points at, so dial the relay.
no_mx_lookups(Relay, Config) ->
    TlsOpts = maps:get(tls_options, Config, []),
    maps:get(no_mx_lookups, Config, false) orelse is_ip_address(Relay) orelse
        proplists:get_value(server_name_indication, TlsOpts) =:= disable.

%% gen_smtp may dial an MX host or a resolved IP address rather than the relay
%% name, and a STARTTLS upgrade has no host name at all, so the relay name is
%% pinned as SNI and as the reference identity for certificate verification.
%% gen_smtp's own ssl defaults include `{depth, 0}`, which rejects any chain
%% with an intermediate, so caller options get a depth unless they set one.
tls_options(Relay, Config) ->
    TlsOpts0 = maps:get(tls_options, Config, default_tls_options()),
    TlsOpts1 =
        case proplists:is_defined(depth, TlsOpts0) of
            true -> TlsOpts0;
            false -> TlsOpts0 ++ [{depth, 3}]
        end,
    case proplists:is_defined(server_name_indication, TlsOpts1) orelse is_ip_address(Relay) of
        true -> TlsOpts1;
        false -> [{server_name_indication, Relay} | TlsOpts1]
    end.

is_ip_address(Host) ->
    element(1, inet:parse_address(Host)) =:= ok.

is_loopback(Host) ->
    case inet:parse_address(Host) of
        {ok, {127, _, _, _}} -> true;
        {ok, {0, 0, 0, 0, 0, 0, 0, 1}} -> true;
        {ok, _} -> false;
        {error, _} -> string:lowercase(Host) =:= "localhost"
    end.

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
        {<<"From">>, hikyaku_address:format(From)},
        {<<"To">>, hikyaku_address:format_list(To)},
        {<<"Subject">>, Subject}
    ],
    Headers1 =
        case Cc of
            [] -> Headers0;
            _ -> Headers0 ++ [{<<"Cc">>, hikyaku_address:format_list(Cc)}]
        end,
    Headers2 =
        case ReplyTo of
            undefined -> Headers1;
            _ -> Headers1 ++ [{<<"Reply-To">>, hikyaku_address:format(ReplyTo)}]
        end,
    Headers3 = Headers2 ++ maps:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], CustomHeaders),
    Headers4 =
        case lists:member(~"message-id", [string:lowercase(K) || K <- maps:keys(CustomHeaders)]) of
            true -> Headers3;
            false -> Headers3 ++ [{~"Message-ID", message_id(From)}]
        end,
    {Type, SubType, BodyHeaders, _Params, SubParts} = build_body(Text, Html, Attachments),
    {Type, SubType, Headers4 ++ BodyHeaders, #{}, SubParts}.

%% mimemail would otherwise generate one from the host FQDN, which leaks the
%% sending machine's name to every recipient.
message_id(#hikyaku_address{address = Address}) ->
    Random = binary:encode_hex(crypto:strong_rand_bytes(16), lowercase),
    <<"<", Random/binary, "@", (message_id_domain(Address))/binary, ">">>.

%% A non-ASCII (IDN) domain would be RFC 2047-encoded by mimemail, which
%% is not a valid msg-id.
message_id_domain(Address) ->
    case binary:split(Address, ~"@") of
        [_, Rest] ->
            message_id_domain(Rest);
        [Domain] ->
            case re:run(Domain, ~"\\A[A-Za-z0-9.-]+\\z") of
                {match, _} -> Domain;
                nomatch -> ~"hikyaku.invalid"
            end
    end.

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

build_attachment_part(
    #hikyaku_attachment{
        filename = Filename,
        content_type = ContentType,
        disposition = Disposition,
        cid = Cid
    } = Att
) ->
    Data = hikyaku_attachment:content(Att),
    {Type, SubType, ContentTypeParams} = hikyaku_attachment:parse_content_type(ContentType),
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
        content_type_params => ContentTypeParams,
        disposition => DispBin,
        disposition_params => [{~"filename", Filename}],
        transfer_encoding => ~"base64"
    },
    {Type, SubType, Headers, Params, Data}.
