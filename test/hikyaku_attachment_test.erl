-module(hikyaku_attachment_test).
-include_lib("eunit/include/eunit.hrl").
-include("hikyaku.hrl").

from_path_test() ->
    Att = hikyaku_attachment:from_path(<<"/tmp/report.pdf">>),
    ?assertEqual(<<"report.pdf">>, Att#hikyaku_attachment.filename),
    ?assertEqual(<<"application/pdf">>, Att#hikyaku_attachment.content_type),
    ?assertEqual(<<"/tmp/report.pdf">>, Att#hikyaku_attachment.path),
    ?assertEqual(undefined, Att#hikyaku_attachment.data),
    ?assertEqual(attachment, Att#hikyaku_attachment.disposition).

from_path_with_opts_test() ->
    Att = hikyaku_attachment:from_path(<<"/tmp/data.bin">>, #{
        content_type => <<"application/x-custom">>,
        filename => <<"renamed.bin">>
    }),
    ?assertEqual(<<"renamed.bin">>, Att#hikyaku_attachment.filename),
    ?assertEqual(<<"application/x-custom">>, Att#hikyaku_attachment.content_type).

from_data_test() ->
    Att = hikyaku_attachment:from_data(<<"hello.txt">>, <<"hello world">>),
    ?assertEqual(<<"hello.txt">>, Att#hikyaku_attachment.filename),
    ?assertEqual(<<"text/plain">>, Att#hikyaku_attachment.content_type),
    ?assertEqual(<<"hello world">>, Att#hikyaku_attachment.data),
    ?assertEqual(undefined, Att#hikyaku_attachment.path).

from_data_with_opts_test() ->
    Att = hikyaku_attachment:from_data(<<"file.bin">>, <<"data">>, #{
        content_type => <<"application/x-custom">>
    }),
    ?assertEqual(<<"application/x-custom">>, Att#hikyaku_attachment.content_type).

inline_test() ->
    Att = hikyaku_attachment:inline(<<"logo123">>, <<"/tmp/logo.png">>),
    ?assertEqual(<<"logo.png">>, Att#hikyaku_attachment.filename),
    ?assertEqual(<<"image/png">>, Att#hikyaku_attachment.content_type),
    ?assertEqual(inline, Att#hikyaku_attachment.disposition),
    ?assertEqual(<<"logo123">>, Att#hikyaku_attachment.cid),
    ?assertEqual(<<"/tmp/logo.png">>, Att#hikyaku_attachment.path).

content_type_detection_test_() ->
    [
        ?_assertEqual(
            <<"image/jpeg">>,
            (hikyaku_attachment:from_data(<<"f.jpg">>, <<>>))#hikyaku_attachment.content_type
        ),
        ?_assertEqual(
            <<"image/png">>,
            (hikyaku_attachment:from_data(<<"f.png">>, <<>>))#hikyaku_attachment.content_type
        ),
        ?_assertEqual(
            <<"text/html">>,
            (hikyaku_attachment:from_data(<<"f.html">>, <<>>))#hikyaku_attachment.content_type
        ),
        ?_assertEqual(
            <<"application/json">>,
            (hikyaku_attachment:from_data(<<"f.json">>, <<>>))#hikyaku_attachment.content_type
        ),
        ?_assertEqual(
            <<"application/zip">>,
            (hikyaku_attachment:from_data(<<"f.zip">>, <<>>))#hikyaku_attachment.content_type
        ),
        ?_assertEqual(
            <<"application/octet-stream">>,
            (hikyaku_attachment:from_data(<<"f.xyz">>, <<>>))#hikyaku_attachment.content_type
        )
    ].

rejects_invalid_content_type_test_() ->
    [
        ?_assertError(
            {invalid_attachment, content_type, CT},
            hikyaku_attachment:from_data(~"a.txt", ~"data", #{content_type => CT})
        )
     || CT <- [
            ~"text/plain\r\nX-CT: 1",
            ~"text/html; charset=x",
            ~"text",
            ~"text/",
            ~"/plain",
            ~"text/plain\n"
        ]
    ].

accepts_token_content_types_test_() ->
    [
        ?_assertEqual(
            CT,
            (hikyaku_attachment:from_data(~"a.bin", ~"d", #{content_type => CT}))#hikyaku_attachment.content_type
        )
     || CT <- [~"application/vnd.ms-excel", ~"image/svg+xml", ~"application/x-custom"]
    ].

rejects_control_characters_in_filename_test_() ->
    [
        ?_assertError(
            {invalid_attachment, filename, _},
            hikyaku_attachment:from_data(~"a.txt\"\r\nX-File: 1", ~"data")
        ),
        ?_assertError(
            {invalid_attachment, filename, _},
            hikyaku_attachment:from_path(~"/tmp/a.txt", #{filename => ~"a\nb.txt"})
        ),
        ?_assertError({invalid_attachment, filename, _}, hikyaku_attachment:from_data(~"", ~"d"))
    ].

rejects_invalid_cid_test_() ->
    [
        ?_assertError(
            {invalid_attachment, cid, Cid}, hikyaku_attachment:inline(Cid, ~"/tmp/logo.png")
        )
     || Cid <- [~"c>\r\nX-Cid: 1", ~"<logo>", ~"lo go", ~""]
    ].
