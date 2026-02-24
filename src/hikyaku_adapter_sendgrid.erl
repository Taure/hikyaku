-module(hikyaku_adapter_sendgrid).
-behaviour(hikyaku_adapter).

-include("hikyaku.hrl").

-export([deliver/2]).

-define(BASE_URL, <<"https://api.sendgrid.com/v3/mail/send">>).

-spec deliver(#hikyaku_email{}, map()) -> {ok, map()} | {error, term()}.
deliver(Email, Config) ->
    #{api_key := ApiKey} = Config,
    HttpMod = maps:get(http_client, Config, default),
    Headers = [
        {<<"authorization">>, <<"Bearer ", ApiKey/binary>>},
        {<<"content-type">>, <<"application/json">>}
    ],
    Body = iolist_to_binary(json:encode(build_payload(Email))),
    case hikyaku_http:post(HttpMod, ?BASE_URL, Headers, Body) of
        {ok, Status, _RespBody} when Status >= 200, Status < 300 ->
            {ok, #{status => sent, status_code => Status}};
        {ok, Status, RespBody} ->
            {error, #{status_code => Status, body => RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%%% Internal

build_payload(#hikyaku_email{
    subject = Subject,
    from = From,
    to = To,
    cc = Cc,
    bcc = Bcc,
    reply_to = ReplyTo,
    text_body = Text,
    html_body = Html
}) ->
    Personalizations = build_personalizations(To, Cc, Bcc),
    Payload0 = #{
        <<"personalizations">> => Personalizations,
        <<"from">> => encode_address(From),
        <<"subject">> => Subject
    },
    Payload1 = maybe_add_content(Payload0, Text, Html),
    Payload2 = maybe_add_reply_to(Payload1, ReplyTo),
    Payload2.

build_personalizations(To, Cc, Bcc) ->
    P0 = #{<<"to">> => [encode_address(A) || A <- To]},
    P1 =
        case Cc of
            [] -> P0;
            _ -> P0#{<<"cc">> => [encode_address(A) || A <- Cc]}
        end,
    P2 =
        case Bcc of
            [] -> P1;
            _ -> P1#{<<"bcc">> => [encode_address(A) || A <- Bcc]}
        end,
    [P2].

encode_address(#hikyaku_address{name = undefined, address = Addr}) ->
    #{<<"email">> => Addr};
encode_address(#hikyaku_address{name = Name, address = Addr}) ->
    #{<<"email">> => Addr, <<"name">> => Name}.

maybe_add_content(Payload, Text, Html) ->
    Content =
        case {Text, Html} of
            {undefined, undefined} ->
                [];
            {T, undefined} when is_binary(T) ->
                [#{<<"type">> => <<"text/plain">>, <<"value">> => T}];
            {undefined, H} when is_binary(H) ->
                [#{<<"type">> => <<"text/html">>, <<"value">> => H}];
            {T, H} when is_binary(T), is_binary(H) ->
                [
                    #{<<"type">> => <<"text/plain">>, <<"value">> => T},
                    #{<<"type">> => <<"text/html">>, <<"value">> => H}
                ]
        end,
    case Content of
        [] -> Payload;
        _ -> Payload#{<<"content">> => Content}
    end.

maybe_add_reply_to(Payload, undefined) ->
    Payload;
maybe_add_reply_to(Payload, ReplyTo) ->
    Payload#{<<"reply_to">> => encode_address(ReplyTo)}.
