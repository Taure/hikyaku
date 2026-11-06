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

escapes_quotes_in_part_names_test() ->
    Parts = [
        {<<"h:X-\"Q\\">>, ~"v"},
        {file, ~"attachment", <<"a.txt\"; x=\"evil.exe">>, ~"text/plain", ~"data"}
    ],
    {_ContentType, Body} = hikyaku_multipart:encode(Parts),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"h:X-\\\"Q\\\\\"\r\n">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"filename=\"a.txt\\\"; x=\\\"evil.exe\"\r\n">>)).

rejects_crlf_in_part_headers_test_() ->
    [
        ?_assertError({invalid_form_part, _}, hikyaku_multipart:encode([{~"a\r\nX: 1", ~"v"}])),
        ?_assertError(
            {invalid_form_part, _},
            hikyaku_multipart:encode([{file, ~"f", ~"a.txt\nX: 1", ~"text/plain", ~"d"}])
        ),
        ?_assertError(
            {invalid_form_part, _},
            hikyaku_multipart:encode([{file, ~"f", ~"a.txt", ~"text/plain\r\nX: 1", ~"d"}])
        )
    ].

field_values_may_contain_newlines_test() ->
    {_ContentType, Body} = hikyaku_multipart:encode([{~"text", ~"line 1\r\nline 2"}]),
    ?assertNotEqual(nomatch, binary:match(Body, ~"line 1\r\nline 2")).
