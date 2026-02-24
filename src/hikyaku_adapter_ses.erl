-module(hikyaku_adapter_ses).
-behaviour(hikyaku_adapter).

-include("hikyaku.hrl").

-export([deliver/2]).

-spec deliver(#hikyaku_email{}, map()) -> {ok, map()} | {error, term()}.
deliver(Email, Config) ->
    #{
        access_key := AccessKey,
        secret_key := SecretKey,
        region := Region
    } = Config,
    HttpMod = maps:get(http_client, Config, default),
    Url = <<"https://email.", Region/binary, ".amazonaws.com/v2/email/outbound-emails">>,
    Body = iolist_to_binary(json:encode(build_payload(Email))),
    BaseHeaders = [{<<"content-type">>, <<"application/json">>}],
    SignOpts = #{
        method => <<"POST">>,
        url => Url,
        headers => BaseHeaders,
        body => Body,
        region => Region,
        service => <<"ses">>,
        access_key => AccessKey,
        secret_key => SecretKey
    },
    SignOpts1 =
        case maps:find(datetime, Config) of
            {ok, DT} -> SignOpts#{datetime => DT};
            error -> SignOpts
        end,
    SignedHeaders = hikyaku_aws_auth:sign_request(SignOpts1),
    case hikyaku_http:post(HttpMod, Url, SignedHeaders, Body) of
        {ok, Status, _RespBody} when Status >= 200, Status < 300 ->
            {ok, #{status => sent, status_code => Status}};
        {ok, Status, RespBody} ->
            {error, #{status_code => Status, body => RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%%% Internal

build_payload(#hikyaku_email{attachments = Attachments} = Email) when Attachments =/= [] ->
    build_raw_payload(Email);
build_payload(Email) ->
    build_simple_payload(Email).

build_simple_payload(#hikyaku_email{
    from = From,
    to = To,
    cc = Cc,
    bcc = Bcc,
    subject = Subject,
    text_body = Text,
    html_body = Html,
    reply_to = ReplyTo
}) ->
    Destination = build_destination(To, Cc, Bcc),
    BodyMap0 = #{},
    BodyMap1 =
        case Text of
            undefined -> BodyMap0;
            _ -> BodyMap0#{<<"Text">> => #{<<"Data">> => Text, <<"Charset">> => <<"UTF-8">>}}
        end,
    BodyMap2 =
        case Html of
            undefined -> BodyMap1;
            _ -> BodyMap1#{<<"Html">> => #{<<"Data">> => Html, <<"Charset">> => <<"UTF-8">>}}
        end,
    Content = #{
        <<"Simple">> => #{
            <<"Subject">> => #{<<"Data">> => Subject, <<"Charset">> => <<"UTF-8">>},
            <<"Body">> => BodyMap2
        }
    },
    Payload0 = #{
        <<"FromEmailAddress">> => format_address(From),
        <<"Destination">> => Destination,
        <<"Content">> => Content
    },
    maybe_add_reply_to(Payload0, ReplyTo).

build_raw_payload(
    #hikyaku_email{
        from = From,
        to = To,
        cc = Cc,
        bcc = Bcc,
        reply_to = ReplyTo
    } = Email
) ->
    MimeTuple = hikyaku_adapter_smtp:build_mimemail(Email),
    MimeBin = mimemail:encode(MimeTuple),
    RawData = base64:encode(MimeBin),
    Destination = build_destination(To, Cc, Bcc),
    Content = #{
        <<"Raw">> => #{<<"Data">> => RawData}
    },
    Payload0 = #{
        <<"FromEmailAddress">> => format_address(From),
        <<"Destination">> => Destination,
        <<"Content">> => Content
    },
    maybe_add_reply_to(Payload0, ReplyTo).

build_destination(To, Cc, Bcc) ->
    D0 = #{<<"ToAddresses">> => [format_address(A) || A <- To]},
    D1 =
        case Cc of
            [] -> D0;
            _ -> D0#{<<"CcAddresses">> => [format_address(A) || A <- Cc]}
        end,
    case Bcc of
        [] -> D1;
        _ -> D1#{<<"BccAddresses">> => [format_address(A) || A <- Bcc]}
    end.

maybe_add_reply_to(Payload, undefined) ->
    Payload;
maybe_add_reply_to(Payload, ReplyTo) ->
    Payload#{<<"ReplyToAddresses">> => [format_address(ReplyTo)]}.

format_address(#hikyaku_address{name = undefined, address = Addr}) ->
    Addr;
format_address(#hikyaku_address{name = Name, address = Addr}) ->
    <<Name/binary, " <", Addr/binary, ">">>.
