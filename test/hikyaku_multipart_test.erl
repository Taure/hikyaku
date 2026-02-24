-module(hikyaku_multipart_test).
-include_lib("eunit/include/eunit.hrl").

encode_text_fields_test() ->
    Parts = [{<<"name">>, <<"value">>}, {<<"key">>, <<"data">>}],
    {ContentType, Body} = hikyaku_multipart:encode(Parts),
    ?assertMatch(<<"multipart/form-data; boundary=", _/binary>>, ContentType),
    ?assertNotEqual(
        nomatch, binary:match(Body, <<"Content-Disposition: form-data; name=\"name\"">>)
    ),
    ?assertNotEqual(nomatch, binary:match(Body, <<"value">>)),
    ?assertNotEqual(
        nomatch, binary:match(Body, <<"Content-Disposition: form-data; name=\"key\"">>)
    ),
    ?assertNotEqual(nomatch, binary:match(Body, <<"data">>)).

encode_file_part_test() ->
    Parts = [{file, <<"attachment">>, <<"test.pdf">>, <<"application/pdf">>, <<"pdfdata">>}],
    {_ContentType, Body} = hikyaku_multipart:encode(Parts),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"attachment\"; filename=\"test.pdf\"">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"Content-Type: application/pdf">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"pdfdata">>)).

encode_mixed_parts_test() ->
    Parts = [
        {<<"field">>, <<"val">>},
        {file, <<"file">>, <<"doc.txt">>, <<"text/plain">>, <<"hello">>}
    ],
    {_ContentType, Body} = hikyaku_multipart:encode(Parts),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"field\"">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"file\"; filename=\"doc.txt\"">>)).

boundary_structure_test() ->
    Parts = [{<<"a">>, <<"b">>}],
    {ContentType, Body} = hikyaku_multipart:encode(Parts),
    <<"multipart/form-data; boundary=", Boundary/binary>> = ContentType,
    ?assertEqual(32, byte_size(Boundary)),
    Opening = <<"--", Boundary/binary>>,
    Closing = <<"--", Boundary/binary, "--\r\n">>,
    ?assertNotEqual(nomatch, binary:match(Body, Opening)),
    ?assertNotEqual(nomatch, binary:match(Body, Closing)).
