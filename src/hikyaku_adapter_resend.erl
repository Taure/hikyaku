-module(hikyaku_adapter_resend).
-behaviour(hikyaku_adapter).

-include("hikyaku.hrl").

-export([deliver/2]).

-define(DEFAULT_BASE_URL, <<"https://api.resend.com">>).

%% Provider options passed straight through to the request payload.
-define(PAYLOAD_OPTIONS, [tags, scheduled_at, topic_id, template]).

-spec deliver(#hikyaku_email{}, map()) -> {ok, map()} | {error, term()}.
deliver(Email = #hikyaku_email{provider_options = Opts}, Config) ->
    #{api_key := ApiKey} = Config,
    HttpMod = maps:get(http_client, Config, default),
    BaseUrl = maps:get(base_url, Config, ?DEFAULT_BASE_URL),
    Url = <<BaseUrl/binary, "/emails">>,
    Headers0 = [
        {<<"authorization">>, <<"Bearer ", ApiKey/binary>>},
        {<<"content-type">>, <<"application/json">>}
    ],
    Headers =
        case maps:find(idempotency_key, Opts) of
            {ok, Key} -> Headers0 ++ [{<<"idempotency-key">>, Key}];
            error -> Headers0
        end,
    Body = iolist_to_binary(json:encode(build_payload(Email))),
    case hikyaku_http:post(HttpMod, Url, Headers, Body) of
        {ok, Status, RespBody} when Status >= 200, Status < 300 ->
            {ok, maybe_add_id(#{status => sent, status_code => Status}, RespBody)};
        {ok, Status, RespBody} ->
            {error, #{status_code => Status, body => RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%%% Internal

build_payload(#hikyaku_email{
    from = From,
    to = To,
    cc = Cc,
    bcc = Bcc,
    reply_to = ReplyTo,
    subject = Subject,
    text_body = Text,
    html_body = Html,
    headers = CustomHeaders,
    attachments = Attachments,
    provider_options = Opts
}) ->
    Payload0 = #{
        <<"from">> => format_address(From),
        <<"to">> => [format_address(A) || A <- To],
        <<"subject">> => Subject
    },
    Optional = [
        {<<"cc">>, [format_address(A) || A <- Cc]},
        {<<"bcc">>, [format_address(A) || A <- Bcc]},
        {<<"reply_to">>,
            case ReplyTo of
                undefined -> [];
                _ -> [format_address(ReplyTo)]
            end},
        {<<"text">>, Text},
        {<<"html">>, Html},
        {<<"headers">>, CustomHeaders},
        {<<"attachments">>, [encode_attachment(A) || A <- Attachments]}
    ],
    Payload1 = lists:foldl(
        fun
            ({_K, undefined}, Acc) -> Acc;
            ({_K, []}, Acc) -> Acc;
            ({_K, Empty}, Acc) when Empty =:= #{} -> Acc;
            ({K, V}, Acc) -> Acc#{K => V}
        end,
        Payload0,
        Optional
    ),
    maps:fold(
        fun(K, V, Acc) -> Acc#{atom_to_binary(K) => V} end,
        Payload1,
        maps:with(?PAYLOAD_OPTIONS, Opts)
    ).

%% Resend's `path` field is a remote URL, so local files are always sent
%% as base64 content.
encode_attachment(#hikyaku_attachment{
    filename = Filename,
    content_type = ContentType,
    data = Data0,
    path = Path,
    disposition = Disposition,
    cid = Cid
}) ->
    Data =
        case Data0 of
            undefined when is_binary(Path) ->
                {ok, Bin} = file:read_file(Path),
                Bin;
            Bin when is_binary(Bin) ->
                Bin
        end,
    Att = #{
        <<"filename">> => Filename,
        <<"content">> => base64:encode(Data),
        <<"content_type">> => ContentType
    },
    case {Disposition, Cid} of
        {inline, C} when is_binary(C) -> Att#{<<"content_id">> => C};
        _ -> Att
    end.

maybe_add_id(Result, RespBody) ->
    try json:decode(RespBody) of
        #{<<"id">> := Id} -> Result#{id => Id};
        _ -> Result
    catch
        error:_ -> Result
    end.

format_address(#hikyaku_address{name = undefined, address = Addr}) ->
    Addr;
format_address(#hikyaku_address{name = Name, address = Addr}) ->
    <<(format_name(Name))/binary, " <", Addr/binary, ">">>.

%% Quote display names containing RFC 5322 specials so that e.g.
%% "Doe, John" is not split into two addresses.
format_name(Name) ->
    case binary:match(Name, [<<C>> || C <- "()<>[]:;@\\,.\""]) of
        nomatch ->
            Name;
        _ ->
            Escaped = binary:replace(
                binary:replace(Name, <<"\\">>, <<"\\\\">>, [global]),
                <<"\"">>,
                <<"\\\"">>,
                [global]
            ),
            <<"\"", Escaped/binary, "\"">>
    end.
