-module(hikyaku_adapter_smtp).
-behaviour(hikyaku_adapter).

-include("hikyaku.hrl").

-export([deliver/2, build_mimemail/1]).

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
    Relay = maps:get(relay, Config),
    Port = maps:get(port, Config, 587),
    Username = maps:get(username, Config, undefined),
    Password = maps:get(password, Config, undefined),
    Opts0 = [
        {relay, binary_to_list(Relay)},
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
    Opts2 =
        case maps:get(ssl, Config, false) of
            true -> Opts1 ++ [{ssl, true}];
            false -> Opts1
        end,
    case maps:get(tls, Config, if_available) of
        always -> Opts2 ++ [{tls, always}];
        never -> Opts2 ++ [{tls, never}];
        if_available -> Opts2 ++ [{tls, if_available}]
    end.

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
        <<"disposition">> => DispBin,
        <<"disposition-params">> => [{<<"filename">>, Filename}],
        <<"transfer-encoding">> => <<"base64">>
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
    lists:join(<<", ">>, Formatted).
