-module(hikyaku_adapter_smtp_test).
-include_lib("eunit/include/eunit.hrl").
-include("hikyaku.hrl").

build_mimemail_text_only_test() ->
    Email = build_email(<<"Hello plain">>, undefined),
    Mime = call_build_mimemail(Email),
    ?assertMatch({<<"text">>, <<"plain">>, _, _, _}, Mime),
    {_, _, _, _, Body} = Mime,
    ?assertEqual(<<"Hello plain">>, Body).

build_mimemail_html_only_test() ->
    Email = build_email(undefined, <<"<b>Hi</b>">>),
    Mime = call_build_mimemail(Email),
    ?assertMatch({<<"text">>, <<"html">>, _, _, _}, Mime).

build_mimemail_multipart_test() ->
    Email = build_email(<<"plain">>, <<"<b>html</b>">>),
    Mime = call_build_mimemail(Email),
    ?assertMatch({<<"multipart">>, <<"alternative">>, _, _, _}, Mime),
    {_, _, _, _, Parts} = Mime,
    ?assertEqual(2, length(Parts)).

build_mimemail_with_attachment_test() ->
    Att = hikyaku_attachment:from_data(<<"file.txt">>, <<"content">>),
    Email0 = build_email(<<"text">>, undefined),
    Email = hikyaku_email:attachment(Email0, Att),
    Mime = call_build_mimemail(Email),
    ?assertMatch({<<"multipart">>, <<"mixed">>, _, _, _}, Mime),
    {_, _, _, _, Parts} = Mime,
    ?assertEqual(2, length(Parts)).

build_mimemail_headers_test() ->
    Email = build_email(<<"text">>, undefined),
    Mime = call_build_mimemail(Email),
    {_, _, Headers, _, _} = Mime,
    FromHeader = proplists:get_value(<<"From">>, Headers),
    ?assertEqual(<<"Alice <alice@example.com>">>, FromHeader),
    SubjectHeader = proplists:get_value(<<"Subject">>, Headers),
    ?assertEqual(<<"Test">>, SubjectHeader).

build_mimemail_cc_header_test() ->
    Email = hikyaku_email:cc(build_email(<<"text">>, undefined), <<"cc@x.com">>),
    Mime = call_build_mimemail(Email),
    {_, _, Headers, _, _} = Mime,
    CcHeader = proplists:get_value(<<"Cc">>, Headers),
    ?assertNotEqual(undefined, CcHeader).

build_mimemail_reply_to_header_test() ->
    Email = hikyaku_email:reply_to(build_email(<<"text">>, undefined), <<"reply@x.com">>),
    Mime = call_build_mimemail(Email),
    {_, _, Headers, _, _} = Mime,
    ReplyToHeader = proplists:get_value(<<"Reply-To">>, Headers),
    ?assertEqual(<<"reply@x.com">>, ReplyToHeader).

%%% Helpers

build_email(Text, Html) ->
    E0 = hikyaku_email:from(hikyaku_email:new(), {<<"Alice">>, <<"alice@example.com">>}),
    E1 = hikyaku_email:to(E0, <<"bob@example.com">>),
    E2 = hikyaku_email:subject(E1, <<"Test">>),
    E3 =
        case Text of
            undefined -> E2;
            _ -> hikyaku_email:text_body(E2, Text)
        end,
    case Html of
        undefined -> E3;
        _ -> hikyaku_email:html_body(E3, Html)
    end.

call_build_mimemail(Email) ->
    %% Access the internal build_mimemail via the module
    %% We test through the exported deliver/2 indirectly,
    %% but for mimemail structure tests we need direct access.
    %% Since build_mimemail is not exported, we use a wrapper approach:
    %% just call mimemail:encode on the result to verify it's valid.
    %%
    %% Actually, let's export build_mimemail for testing.
    %% For now, test by encoding and checking it doesn't crash.
    hikyaku_adapter_smtp:build_mimemail(Email).
