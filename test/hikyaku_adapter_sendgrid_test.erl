-module(hikyaku_adapter_sendgrid_test).
-include_lib("eunit/include/eunit.hrl").
-include("hikyaku.hrl").

-export([post/3]).

%% Mock HTTP client
post(_Url, _Headers, Body) ->
    persistent_term:put(sendgrid_test_body, Body),
    {ok, 202, <<>>}.

deliver_success_test() ->
    Email = build_email(),
    Config = #{
        adapter => hikyaku_adapter_sendgrid,
        api_key => <<"test-key">>,
        http_client => ?MODULE
    },
    {ok, #{status := sent, status_code := 202}} = hikyaku_adapter_sendgrid:deliver(Email, Config),
    Body = persistent_term:get(sendgrid_test_body),
    Decoded = json:decode(Body),
    ?assertEqual(<<"Hello">>, maps:get(<<"subject">>, Decoded)),
    From = maps:get(<<"from">>, Decoded),
    ?assertEqual(<<"alice@example.com">>, maps:get(<<"email">>, From)),
    ?assertEqual(<<"Alice">>, maps:get(<<"name">>, From)),
    [Personalization] = maps:get(<<"personalizations">>, Decoded),
    [ToAddr] = maps:get(<<"to">>, Personalization),
    ?assertEqual(<<"bob@example.com">>, maps:get(<<"email">>, ToAddr)).

deliver_with_content_test() ->
    Email0 = build_email(),
    Email = hikyaku_email:html_body(
        hikyaku_email:text_body(Email0, <<"plain text">>),
        <<"<b>html</b>">>
    ),
    Config = #{
        adapter => hikyaku_adapter_sendgrid,
        api_key => <<"test-key">>,
        http_client => ?MODULE
    },
    {ok, _} = hikyaku_adapter_sendgrid:deliver(Email, Config),
    Body = persistent_term:get(sendgrid_test_body),
    Decoded = json:decode(Body),
    Content = maps:get(<<"content">>, Decoded),
    ?assertEqual(2, length(Content)).

deliver_with_cc_bcc_test() ->
    Email = hikyaku_email:bcc(
        hikyaku_email:cc(build_email(), <<"cc@x.com">>),
        <<"bcc@x.com">>
    ),
    Config = #{
        adapter => hikyaku_adapter_sendgrid,
        api_key => <<"test-key">>,
        http_client => ?MODULE
    },
    {ok, _} = hikyaku_adapter_sendgrid:deliver(Email, Config),
    Body = persistent_term:get(sendgrid_test_body),
    Decoded = json:decode(Body),
    [Personalization] = maps:get(<<"personalizations">>, Decoded),
    ?assert(maps:is_key(<<"cc">>, Personalization)),
    ?assert(maps:is_key(<<"bcc">>, Personalization)).

deliver_with_reply_to_test() ->
    Email = hikyaku_email:reply_to(build_email(), <<"reply@x.com">>),
    Config = #{
        adapter => hikyaku_adapter_sendgrid,
        api_key => <<"test-key">>,
        http_client => ?MODULE
    },
    {ok, _} = hikyaku_adapter_sendgrid:deliver(Email, Config),
    Body = persistent_term:get(sendgrid_test_body),
    Decoded = json:decode(Body),
    ReplyTo = maps:get(<<"reply_to">>, Decoded),
    ?assertEqual(<<"reply@x.com">>, maps:get(<<"email">>, ReplyTo)).

deliver_http_error_test() ->
    Email = build_email(),
    Config = #{
        adapter => hikyaku_adapter_sendgrid,
        api_key => <<"test-key">>,
        http_client => hikyaku_adapter_sendgrid_error_mock
    },
    {error, #{status_code := 400}} = hikyaku_adapter_sendgrid:deliver(Email, Config).

%%% Helpers

build_email() ->
    hikyaku_email:subject(
        hikyaku_email:to(
            hikyaku_email:from(hikyaku_email:new(), {<<"Alice">>, <<"alice@example.com">>}),
            <<"bob@example.com">>
        ),
        <<"Hello">>
    ).
