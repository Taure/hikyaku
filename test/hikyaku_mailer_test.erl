-module(hikyaku_mailer_test).
-include_lib("eunit/include/eunit.hrl").
-include("hikyaku.hrl").

-behaviour(hikyaku_mailer).
-export([config/0]).

config() ->
    #{adapter => hikyaku_adapter_test, pid => self()}.

deliver_success_test() ->
    Email = build_valid_email(),
    {ok, #{status := sent}} = hikyaku_mailer:deliver(?MODULE, Email),
    receive
        {hikyaku_email, Received} ->
            ?assertEqual(<<"Hello">>, Received#hikyaku_email.subject)
    after 1000 ->
        ?assert(false)
    end.

deliver_missing_from_test() ->
    Email = hikyaku_email:to(hikyaku_email:new(), <<"bob@x.com">>),
    ?assertEqual(
        {error, {validation, <<"from is required">>}},
        hikyaku_mailer:deliver(?MODULE, Email)
    ).

deliver_missing_recipients_test() ->
    Email = hikyaku_email:from(hikyaku_email:new(), <<"alice@x.com">>),
    ?assertEqual(
        {error, {validation, <<"at least one recipient (to, cc, or bcc) is required">>}},
        hikyaku_mailer:deliver(?MODULE, Email)
    ).

deliver_cc_only_valid_test() ->
    Email = hikyaku_email:cc(
        hikyaku_email:from(hikyaku_email:new(), <<"alice@x.com">>),
        <<"bob@x.com">>
    ),
    {ok, _} = hikyaku_mailer:deliver(?MODULE, Email).

deliver_bcc_only_valid_test() ->
    Email = hikyaku_email:bcc(
        hikyaku_email:from(hikyaku_email:new(), <<"alice@x.com">>),
        <<"bob@x.com">>
    ),
    {ok, _} = hikyaku_mailer:deliver(?MODULE, Email).

%%% Helpers

build_valid_email() ->
    hikyaku_email:text_body(
        hikyaku_email:subject(
            hikyaku_email:to(
                hikyaku_email:from(hikyaku_email:new(), {<<"Alice">>, <<"alice@example.com">>}),
                <<"bob@example.com">>
            ),
            <<"Hello">>
        ),
        <<"Hi Bob">>
    ).
