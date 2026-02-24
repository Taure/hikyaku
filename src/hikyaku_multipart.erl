-module(hikyaku_multipart).

-export([encode/1]).

-spec encode([{binary(), binary()} | {file, binary(), binary(), binary(), binary()}]) ->
    {binary(), binary()}.
encode(Parts) ->
    Boundary = generate_boundary(),
    Body = encode_parts(Parts, Boundary),
    ContentType = <<"multipart/form-data; boundary=", Boundary/binary>>,
    {ContentType, Body}.

%%% Internal

generate_boundary() ->
    Bytes = crypto:strong_rand_bytes(16),
    hex_encode(Bytes).

hex_encode(Bin) ->
    <<<<(hex_char(H)), (hex_char(L))>> || <<N>> <= Bin, H <- [N bsr 4], L <- [N band 16#0F]>>.

hex_char(N) when N < 10 -> N + $0;
hex_char(N) -> N - 10 + $a.

encode_parts(Parts, Boundary) ->
    Encoded = [encode_part(P, Boundary) || P <- Parts],
    Closing = <<"--", Boundary/binary, "--\r\n">>,
    iolist_to_binary([Encoded, Closing]).

encode_part({Name, Value}, Boundary) ->
    [
        <<"--", Boundary/binary, "\r\n">>,
        <<"Content-Disposition: form-data; name=\"", Name/binary, "\"\r\n">>,
        <<"\r\n">>,
        Value,
        <<"\r\n">>
    ];
encode_part({file, Name, Filename, ContentType, Data}, Boundary) ->
    [
        <<"--", Boundary/binary, "\r\n">>,
        <<"Content-Disposition: form-data; name=\"", Name/binary, "\"; filename=\"",
            Filename/binary, "\"\r\n">>,
        <<"Content-Type: ", ContentType/binary, "\r\n">>,
        <<"\r\n">>,
        Data,
        <<"\r\n">>
    ].
