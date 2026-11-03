-module(hikyaku_adapter_ses_test).
-include_lib("eunit/include/eunit.hrl").
-include("hikyaku.hrl").

-export([post/3]).

%% Mock HTTP client
post(Url, Headers, Body) ->
    persistent_term:put(ses_test_url, Url),
    persistent_term:put(ses_test_headers, Headers),
    persistent_term:put(ses_test_body, Body),
    {ok, 200, <<"{\"MessageId\":\"abc-123\"}">>}.

deliver_success_test() ->
    Email = build_email(),
    Config = base_config(),
    {ok, #{status := sent, status_code := 200}} = hikyaku_adapter_ses:deliver(Email, Config),
    Body = persistent_term:get(ses_test_body),
    Decoded = json:decode(Body),
    Content = maps:get(<<"Content">>, Decoded),
    SimpleContent = maps:get(<<"Simple">>, Content),
    SubjectMap = maps:get(<<"Subject">>, SimpleContent),
    ?assertEqual(<<"Hello">>, maps:get(<<"Data">>, SubjectMap)),
    ?assertEqual(<<"Alice <alice@example.com>">>, maps:get(<<"FromEmailAddress">>, Decoded)),
    Dest = maps:get(<<"Destination">>, Decoded),
    [ToAddr] = maps:get(<<"ToAddresses">>, Dest),
    ?assertEqual(<<"bob@example.com">>, ToAddr),
    Headers = persistent_term:get(ses_test_headers),
    AuthHeader = proplists:get_value(<<"authorization">>, Headers),
    ?assertMatch(<<"AWS4-HMAC-SHA256 ", _/binary>>, AuthHeader),
    Url = persistent_term:get(ses_test_url),
    ?assertMatch(<<"https://email.us-east-1.amazonaws.com/", _/binary>>, Url).

deliver_with_cc_bcc_test() ->
    Email = hikyaku_email:bcc(
        hikyaku_email:cc(build_email(), <<"cc@x.com">>),
        <<"bcc@x.com">>
    ),
    Config = base_config(),
    {ok, _} = hikyaku_adapter_ses:deliver(Email, Config),
    Body = persistent_term:get(ses_test_body),
    Decoded = json:decode(Body),
    Dest = maps:get(<<"Destination">>, Decoded),
    ?assert(maps:is_key(<<"CcAddresses">>, Dest)),
    ?assert(maps:is_key(<<"BccAddresses">>, Dest)).

deliver_with_html_test() ->
    Email = hikyaku_email:html_body(build_email(), <<"<b>bold</b>">>),
    Config = base_config(),
    {ok, _} = hikyaku_adapter_ses:deliver(Email, Config),
    Body = persistent_term:get(ses_test_body),
    Decoded = json:decode(Body),
    SimpleContent = maps:get(<<"Simple">>, maps:get(<<"Content">>, Decoded)),
    BodyMap = maps:get(<<"Body">>, SimpleContent),
    ?assert(maps:is_key(<<"Html">>, BodyMap)),
    ?assertEqual(<<"<b>bold</b>">>, maps:get(<<"Data">>, maps:get(<<"Html">>, BodyMap))).

deliver_with_attachments_test() ->
    Att = hikyaku_attachment:from_data(<<"test.txt">>, <<"file content">>),
    Email = hikyaku_email:attachment(build_email(), Att),
    Config = base_config(),
    {ok, _} = hikyaku_adapter_ses:deliver(Email, Config),
    Body = persistent_term:get(ses_test_body),
    Decoded = json:decode(Body),
    Content = maps:get(<<"Content">>, Decoded),
    ?assert(maps:is_key(<<"Raw">>, Content)),
    RawData = maps:get(<<"Data">>, maps:get(<<"Raw">>, Content)),
    ?assert(is_binary(RawData)),
    ?assert(byte_size(RawData) > 0).

deliver_raw_attachment_keeps_filename_and_base64_test() ->
    Data = <<0, 1, 2, 255, "binary payload">>,
    Att = hikyaku_attachment:from_data(~"report.pdf", Data),
    {ok, _} = hikyaku_adapter_ses:deliver(
        hikyaku_email:attachment(build_email(), Att), base_config()
    ),
    Raw = base64:decode(raw_data(persistent_term:get(ses_test_body))),
    ?assertMatch(
        {match, _},
        re:run(Raw, ~"Content-Disposition: attachment;\\s*filename=\"?report.pdf", [caseless])
    ),
    ?assertMatch({match, _}, re:run(Raw, ~"Content-Transfer-Encoding: base64", [caseless])),
    {_, _, _, _, Parts} = mimemail:decode(Raw),
    [{_, _, _, Params, Body}] = [P || {_, _, _, #{disposition := ~"attachment"}, _} = P <- Parts],
    ?assertEqual(
        ~"report.pdf", proplists:get_value(~"filename", maps:get(disposition_params, Params))
    ),
    ?assertEqual(Data, Body).

deliver_with_reply_to_test() ->
    Email = hikyaku_email:reply_to(build_email(), <<"reply@x.com">>),
    Config = base_config(),
    {ok, _} = hikyaku_adapter_ses:deliver(Email, Config),
    Body = persistent_term:get(ses_test_body),
    Decoded = json:decode(Body),
    ?assert(maps:is_key(<<"ReplyToAddresses">>, Decoded)),
    [ReplyAddr] = maps:get(<<"ReplyToAddresses">>, Decoded),
    ?assertEqual(<<"reply@x.com">>, ReplyAddr).

deliver_error_test() ->
    Email = build_email(),
    Config = (base_config())#{http_client => hikyaku_adapter_ses_error_mock},
    {error, #{status_code := 400}} = hikyaku_adapter_ses:deliver(Email, Config).

%%% Helpers

raw_data(Body) ->
    #{~"Content" := #{~"Raw" := #{~"Data" := Data}}} = json:decode(Body),
    Data.

base_config() ->
    #{
        adapter => hikyaku_adapter_ses,
        access_key => <<"AKIAIOSFODNN7EXAMPLE">>,
        secret_key => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
        region => <<"us-east-1">>,
        datetime => {{2024, 1, 15}, {12, 0, 0}},
        http_client => ?MODULE
    }.

build_email() ->
    hikyaku_email:text_body(
        hikyaku_email:subject(
            hikyaku_email:to(
                hikyaku_email:from(hikyaku_email:new(), {<<"Alice">>, <<"alice@example.com">>}),
                <<"bob@example.com">>
            ),
            <<"Hello">>
        ),
        <<"Hello Bob">>
    ).
