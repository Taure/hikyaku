-module(hikyaku_aws_auth).

-export([sign_request/1]).

-spec sign_request(#{
    method := binary(),
    url := binary(),
    headers := [{binary(), binary()}],
    body := binary(),
    region := binary(),
    service := binary(),
    access_key := binary(),
    secret_key := binary(),
    datetime => calendar:datetime()
}) -> [{binary(), binary()}].
sign_request(Opts) ->
    #{
        method := Method,
        url := Url,
        headers := Headers0,
        body := Body,
        region := Region,
        service := Service,
        access_key := AccessKey,
        secret_key := SecretKey
    } = Opts,
    DateTime = maps:get(datetime, Opts, calendar:universal_time()),
    {DateStamp, TimeStamp} = format_datetime(DateTime),
    AmzDate = <<DateStamp/binary, "T", TimeStamp/binary, "Z">>,
    PayloadHash = hex_sha256(Body),
    {Host, Path, Query} = parse_url(Url),
    Headers1 = [
        {<<"host">>, Host},
        {<<"x-amz-date">>, AmzDate},
        {<<"x-amz-content-sha256">>, PayloadHash}
        | Headers0
    ],
    SortedHeaders = lists:sort(fun({K1, _}, {K2, _}) -> K1 =< K2 end, Headers1),
    SignedHeaderKeys = lists:join(
        <<$;>>, [string:lowercase(K) || {K, _} <- SortedHeaders]
    ),
    SignedHeaders = iolist_to_binary(SignedHeaderKeys),
    CanonicalHeaders = iolist_to_binary([
        [string:lowercase(K), <<":">>, V, <<"\n">>]
     || {K, V} <- SortedHeaders
    ]),
    CanonicalRequest = iolist_to_binary([
        Method,
        <<"\n">>,
        Path,
        <<"\n">>,
        Query,
        <<"\n">>,
        CanonicalHeaders,
        <<"\n">>,
        SignedHeaders,
        <<"\n">>,
        PayloadHash
    ]),
    CredentialScope =
        <<DateStamp/binary, "/", Region/binary, "/", Service/binary, "/aws4_request">>,
    StringToSign = iolist_to_binary([
        <<"AWS4-HMAC-SHA256\n">>,
        AmzDate,
        <<"\n">>,
        CredentialScope,
        <<"\n">>,
        hex_sha256(CanonicalRequest)
    ]),
    SigningKey = derive_signing_key(SecretKey, DateStamp, Region, Service),
    Signature = hex_encode(hmac_sha256(SigningKey, StringToSign)),
    Authorization = iolist_to_binary([
        <<"AWS4-HMAC-SHA256 Credential=">>,
        AccessKey,
        <<"/">>,
        CredentialScope,
        <<", SignedHeaders=">>,
        SignedHeaders,
        <<", Signature=">>,
        Signature
    ]),
    [{<<"authorization">>, Authorization} | Headers1].

%%% Internal

derive_signing_key(SecretKey, DateStamp, Region, Service) ->
    K0 = <<"AWS4", SecretKey/binary>>,
    K1 = hmac_sha256(K0, DateStamp),
    K2 = hmac_sha256(K1, Region),
    K3 = hmac_sha256(K2, Service),
    hmac_sha256(K3, <<"aws4_request">>).

hmac_sha256(Key, Data) ->
    crypto:mac(hmac, sha256, Key, Data).

hex_sha256(Data) ->
    hex_encode(crypto:hash(sha256, Data)).

hex_encode(Bin) ->
    <<<<(hex_char(H)), (hex_char(L))>> || <<N>> <= Bin, H <- [N bsr 4], L <- [N band 16#0F]>>.

hex_char(N) when N < 10 -> N + $0;
hex_char(N) -> N - 10 + $a.

format_datetime({{Y, Mo, D}, {H, Mi, S}}) ->
    DateStamp = iolist_to_binary(
        io_lib:format("~4..0B~2..0B~2..0B", [Y, Mo, D])
    ),
    TimeStamp = iolist_to_binary(
        io_lib:format("~2..0B~2..0B~2..0B", [H, Mi, S])
    ),
    {DateStamp, TimeStamp}.

parse_url(Url) ->
    #{host := Host, path := Path} = uri_string:parse(Url),
    Query = maps:get(query, uri_string:parse(Url), <<>>),
    NormalizedPath =
        case Path of
            <<>> -> <<"/">>;
            _ -> Path
        end,
    {Host, NormalizedPath, Query}.
