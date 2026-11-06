-module(hikyaku_adapter_mailgun).
-behaviour(hikyaku_adapter).

-include("hikyaku.hrl").

-export([deliver/2]).

-define(DEFAULT_BASE_URL, <<"https://api.mailgun.net">>).

-spec deliver(#hikyaku_email{}, map()) -> {ok, map()} | {error, term()}.
deliver(Email, Config) ->
    #{api_key := ApiKey, domain := Domain} = Config,
    HttpMod = maps:get(http_client, Config, default),
    BaseUrl = maps:get(base_url, Config, ?DEFAULT_BASE_URL),
    EncodedDomain = hikyaku_url:encode(Domain),
    Url = <<BaseUrl/binary, "/v3/", EncodedDomain/binary, "/messages">>,
    Credentials = base64:encode(<<"api:", ApiKey/binary>>),
    try hikyaku_multipart:encode(build_parts(Email)) of
        {ContentType, Body} -> post(HttpMod, Url, Credentials, ContentType, Body)
    catch
        error:{invalid_form_part, _} = Reason -> {error, Reason}
    end.

post(HttpMod, Url, Credentials, ContentType, Body) ->
    Headers = [
        {<<"authorization">>, <<"Basic ", Credentials/binary>>},
        {<<"content-type">>, ContentType}
    ],
    case hikyaku_http:post(HttpMod, Url, Headers, Body) of
        {ok, Status, _RespBody} when Status >= 200, Status < 300 ->
            {ok, #{status => sent, status_code => Status}};
        {ok, Status, RespBody} ->
            {error, #{status_code => Status, body => RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%%% Internal

build_parts(#hikyaku_email{
    from = From,
    to = To,
    cc = Cc,
    bcc = Bcc,
    subject = Subject,
    text_body = Text,
    html_body = Html,
    reply_to = ReplyTo,
    headers = CustomHeaders,
    attachments = Attachments
}) ->
    Parts0 = [
        {<<"from">>, hikyaku_address:format(From)},
        {<<"to">>, hikyaku_address:format_list(To)},
        {<<"subject">>, Subject}
    ],
    Parts1 =
        case Cc of
            [] -> Parts0;
            _ -> Parts0 ++ [{<<"cc">>, hikyaku_address:format_list(Cc)}]
        end,
    Parts2 =
        case Bcc of
            [] -> Parts1;
            _ -> Parts1 ++ [{<<"bcc">>, hikyaku_address:format_list(Bcc)}]
        end,
    Parts3 =
        case Text of
            undefined -> Parts2;
            _ -> Parts2 ++ [{<<"text">>, Text}]
        end,
    Parts4 =
        case Html of
            undefined -> Parts3;
            _ -> Parts3 ++ [{<<"html">>, Html}]
        end,
    Parts5 =
        case ReplyTo of
            undefined -> Parts4;
            _ -> Parts4 ++ [{<<"h:Reply-To">>, hikyaku_address:format(ReplyTo)}]
        end,
    Parts6 = maps:fold(
        fun(K, V, Acc) ->
            Acc ++ [{<<"h:", K/binary>>, V}]
        end,
        Parts5,
        CustomHeaders
    ),
    Parts6 ++ build_attachment_parts(Attachments).

build_attachment_parts(Attachments) ->
    lists:map(fun build_attachment_part/1, Attachments).

build_attachment_part(
    #hikyaku_attachment{
        filename = Filename,
        content_type = ContentType,
        disposition = Disposition
    } = Att
) ->
    Data = hikyaku_attachment:content(Att),
    FieldName =
        case Disposition of
            inline -> <<"inline">>;
            attachment -> <<"attachment">>
        end,
    {file, FieldName, Filename, ContentType, Data}.
