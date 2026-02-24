-module(hikyaku_adapter_mailgun_test).
-include_lib("eunit/include/eunit.hrl").
-include("hikyaku.hrl").

-export([post/3]).

%% Mock HTTP client
post(Url, Headers, Body) ->
    persistent_term:put(mailgun_test_url, Url),
    persistent_term:put(mailgun_test_headers, Headers),
    persistent_term:put(mailgun_test_body, Body),
    {ok, 200, <<"{\"id\":\"<123@mg.example.com>\",\"message\":\"Queued\"}">>}.

deliver_success_test() ->
    Email = build_email(),
    Config = base_config(),
    {ok, #{status := sent, status_code := 200}} = hikyaku_adapter_mailgun:deliver(Email, Config),
    Body = persistent_term:get(mailgun_test_body),
    Headers = persistent_term:get(mailgun_test_headers),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"from\"">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"alice@example.com">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"to\"">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"bob@example.com">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"subject\"">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"Hello">>)),
    AuthHeader = proplists:get_value(<<"authorization">>, Headers),
    ?assertMatch(<<"Basic ", _/binary>>, AuthHeader).

deliver_with_cc_bcc_test() ->
    Email = hikyaku_email:bcc(
        hikyaku_email:cc(build_email(), <<"cc@x.com">>),
        <<"bcc@x.com">>
    ),
    Config = base_config(),
    {ok, _} = hikyaku_adapter_mailgun:deliver(Email, Config),
    Body = persistent_term:get(mailgun_test_body),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"cc\"">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"cc@x.com">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"bcc\"">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"bcc@x.com">>)).

deliver_with_html_test() ->
    Email = hikyaku_email:html_body(build_email(), <<"<b>bold</b>">>),
    Config = base_config(),
    {ok, _} = hikyaku_adapter_mailgun:deliver(Email, Config),
    Body = persistent_term:get(mailgun_test_body),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"html\"">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"<b>bold</b>">>)).

deliver_with_attachments_test() ->
    Att = hikyaku_attachment:from_data(<<"test.txt">>, <<"file content">>),
    Email = hikyaku_email:attachment(build_email(), Att),
    Config = base_config(),
    {ok, _} = hikyaku_adapter_mailgun:deliver(Email, Config),
    Body = persistent_term:get(mailgun_test_body),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"attachment\"; filename=\"test.txt\"">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"file content">>)).

deliver_with_inline_attachment_test() ->
    Att = #hikyaku_attachment{
        filename = <<"logo.png">>,
        content_type = <<"image/png">>,
        data = <<"pngdata">>,
        disposition = inline
    },
    Email = hikyaku_email:attachment(build_email(), Att),
    Config = base_config(),
    {ok, _} = hikyaku_adapter_mailgun:deliver(Email, Config),
    Body = persistent_term:get(mailgun_test_body),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"inline\"; filename=\"logo.png\"">>)).

deliver_with_custom_headers_test() ->
    Email = hikyaku_email:header(build_email(), <<"X-Custom">>, <<"custom-value">>),
    Config = base_config(),
    {ok, _} = hikyaku_adapter_mailgun:deliver(Email, Config),
    Body = persistent_term:get(mailgun_test_body),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"h:X-Custom\"">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"custom-value">>)).

deliver_with_reply_to_test() ->
    Email = hikyaku_email:reply_to(build_email(), <<"reply@x.com">>),
    Config = base_config(),
    {ok, _} = hikyaku_adapter_mailgun:deliver(Email, Config),
    Body = persistent_term:get(mailgun_test_body),
    ?assertNotEqual(nomatch, binary:match(Body, <<"name=\"h:Reply-To\"">>)),
    ?assertNotEqual(nomatch, binary:match(Body, <<"reply@x.com">>)).

deliver_eu_region_test() ->
    Email = build_email(),
    Config = (base_config())#{base_url => <<"https://api.eu.mailgun.net">>},
    {ok, _} = hikyaku_adapter_mailgun:deliver(Email, Config),
    Url = persistent_term:get(mailgun_test_url),
    ?assertMatch(<<"https://api.eu.mailgun.net/v3/", _/binary>>, Url).

deliver_error_test() ->
    Email = build_email(),
    Config = (base_config())#{http_client => hikyaku_adapter_mailgun_error_mock},
    {error, #{status_code := 400}} = hikyaku_adapter_mailgun:deliver(Email, Config).

%%% Helpers

base_config() ->
    #{
        adapter => hikyaku_adapter_mailgun,
        api_key => <<"key-test123">>,
        domain => <<"mg.example.com">>,
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
