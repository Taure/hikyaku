-module(hikyaku_adapter_resend_test).
-include_lib("eunit/include/eunit.hrl").
-include("hikyaku.hrl").

-export([post/3]).

%% Mock HTTP client
post(Url, Headers, Body) ->
    persistent_term:put(resend_test_url, Url),
    persistent_term:put(resend_test_headers, Headers),
    persistent_term:put(resend_test_body, Body),
    {ok, 200, <<"{\"id\":\"49a3999c-0ce1-4ea6-ab68-afcd6dc2e794\"}">>}.

deliver_success_test() ->
    {ok, Result} = hikyaku_adapter_resend:deliver(build_email(), base_config()),
    ?assertMatch(
        #{status := sent, status_code := 200, id := <<"49a3999c-0ce1-4ea6-ab68-afcd6dc2e794">>},
        Result
    ),
    ?assertEqual(<<"https://api.resend.com/emails">>, persistent_term:get(resend_test_url)),
    Headers = persistent_term:get(resend_test_headers),
    ?assertEqual(
        {<<"authorization">>, <<"Bearer re_test">>},
        lists:keyfind(<<"authorization">>, 1, Headers)
    ),
    ?assertEqual(false, lists:keyfind(<<"idempotency-key">>, 1, Headers)),
    Decoded = decoded_body(),
    ?assertEqual(<<"Alice <alice@example.com>">>, maps:get(<<"from">>, Decoded)),
    ?assertEqual([<<"bob@example.com">>], maps:get(<<"to">>, Decoded)),
    ?assertEqual(<<"Hello">>, maps:get(<<"subject">>, Decoded)),
    ?assertEqual(<<"Hello Bob">>, maps:get(<<"text">>, Decoded)),
    [
        ?assertNot(maps:is_key(K, Decoded))
     || K <- [<<"cc">>, <<"bcc">>, <<"reply_to">>, <<"html">>, <<"headers">>, <<"attachments">>]
    ].

deliver_with_cc_bcc_reply_to_test() ->
    Email = hikyaku_email:reply_to(
        hikyaku_email:bcc(hikyaku_email:cc(build_email(), <<"cc@x.com">>), <<"bcc@x.com">>),
        <<"reply@x.com">>
    ),
    {ok, _} = hikyaku_adapter_resend:deliver(Email, base_config()),
    Decoded = decoded_body(),
    ?assertEqual([<<"cc@x.com">>], maps:get(<<"cc">>, Decoded)),
    ?assertEqual([<<"bcc@x.com">>], maps:get(<<"bcc">>, Decoded)),
    ?assertEqual([<<"reply@x.com">>], maps:get(<<"reply_to">>, Decoded)).

deliver_with_html_test() ->
    Email = hikyaku_email:html_body(build_email(), <<"<b>Hi</b>">>),
    {ok, _} = hikyaku_adapter_resend:deliver(Email, base_config()),
    ?assertEqual(<<"<b>Hi</b>">>, maps:get(<<"html">>, decoded_body())).

deliver_quotes_display_name_test() ->
    Email = hikyaku_email:put_to(build_email(), [{<<"Doe, \"JD\" John">>, <<"jd@x.com">>}]),
    {ok, _} = hikyaku_adapter_resend:deliver(Email, base_config()),
    ?assertEqual(
        [<<"\"Doe, \\\"JD\\\" John\" <jd@x.com>">>],
        maps:get(<<"to">>, decoded_body())
    ).

deliver_with_custom_headers_test() ->
    Email = hikyaku_email:header(build_email(), <<"X-Custom">>, <<"custom-value">>),
    {ok, _} = hikyaku_adapter_resend:deliver(Email, base_config()),
    ?assertEqual(
        #{<<"X-Custom">> => <<"custom-value">>},
        maps:get(<<"headers">>, decoded_body())
    ).

deliver_with_attachment_test() ->
    Att = hikyaku_attachment:from_data(<<"data.csv">>, <<"a,b\n1,2">>),
    Email = hikyaku_email:attachment(build_email(), Att),
    {ok, _} = hikyaku_adapter_resend:deliver(Email, base_config()),
    [Encoded] = maps:get(<<"attachments">>, decoded_body()),
    ?assertEqual(
        #{
            <<"filename">> => <<"data.csv">>,
            <<"content">> => base64:encode(<<"a,b\n1,2">>),
            <<"content_type">> => <<"text/csv">>
        },
        Encoded
    ).

deliver_with_path_attachment_test() ->
    Path = iolist_to_binary(
        filename:join(
            filename:basedir(user_cache, "hikyaku_test"), "resend_report.txt"
        )
    ),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, <<"report">>),
    Email = hikyaku_email:attachment(build_email(), hikyaku_attachment:from_path(Path)),
    {ok, _} = hikyaku_adapter_resend:deliver(Email, base_config()),
    ok = file:delete(Path),
    [Encoded] = maps:get(<<"attachments">>, decoded_body()),
    ?assertEqual(base64:encode(<<"report">>), maps:get(<<"content">>, Encoded)),
    ?assertNot(maps:is_key(<<"path">>, Encoded)).

deliver_with_inline_attachment_test() ->
    Att = #hikyaku_attachment{
        filename = <<"logo.png">>,
        content_type = <<"image/png">>,
        data = <<"pngdata">>,
        disposition = inline,
        cid = <<"logo">>
    },
    Email = hikyaku_email:attachment(build_email(), Att),
    {ok, _} = hikyaku_adapter_resend:deliver(Email, base_config()),
    [Encoded] = maps:get(<<"attachments">>, decoded_body()),
    ?assertEqual(<<"logo">>, maps:get(<<"content_id">>, Encoded)).

deliver_with_provider_options_test() ->
    Email = lists:foldl(
        fun({K, V}, E) -> hikyaku_email:provider_option(E, K, V) end,
        build_email(),
        [
            {idempotency_key, <<"welcome-user/123">>},
            {tags, [#{name => <<"category">>, value => <<"welcome">>}]},
            {scheduled_at, <<"in 1 min">>},
            {unknown_option, <<"ignored">>}
        ]
    ),
    {ok, _} = hikyaku_adapter_resend:deliver(Email, base_config()),
    ?assertEqual(
        {<<"idempotency-key">>, <<"welcome-user/123">>},
        lists:keyfind(<<"idempotency-key">>, 1, persistent_term:get(resend_test_headers))
    ),
    Decoded = decoded_body(),
    ?assertEqual(
        [#{<<"name">> => <<"category">>, <<"value">> => <<"welcome">>}],
        maps:get(<<"tags">>, Decoded)
    ),
    ?assertEqual(<<"in 1 min">>, maps:get(<<"scheduled_at">>, Decoded)),
    ?assertNot(maps:is_key(<<"idempotency_key">>, Decoded)),
    ?assertNot(maps:is_key(<<"unknown_option">>, Decoded)).

deliver_custom_base_url_test() ->
    Config = (base_config())#{base_url => <<"http://localhost:4000">>},
    {ok, _} = hikyaku_adapter_resend:deliver(build_email(), Config),
    ?assertEqual(<<"http://localhost:4000/emails">>, persistent_term:get(resend_test_url)).

deliver_error_test() ->
    Config = (base_config())#{http_client => hikyaku_adapter_resend_error_mock},
    {error, #{status_code := 422, body := Body}} =
        hikyaku_adapter_resend:deliver(build_email(), Config),
    ?assertMatch(#{<<"name">> := <<"validation_error">>}, json:decode(Body)).

%%% Helpers

decoded_body() ->
    json:decode(persistent_term:get(resend_test_body)).

base_config() ->
    #{
        adapter => hikyaku_adapter_resend,
        api_key => <<"re_test">>,
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
