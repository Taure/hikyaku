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
