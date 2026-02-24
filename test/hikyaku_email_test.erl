-module(hikyaku_email_test).
-include_lib("eunit/include/eunit.hrl").
-include("hikyaku.hrl").

new_test() ->
    Email = hikyaku_email:new(),
    ?assertEqual(<<>>, Email#hikyaku_email.subject),
    ?assertEqual(undefined, Email#hikyaku_email.from),
    ?assertEqual([], Email#hikyaku_email.to),
    ?assertEqual([], Email#hikyaku_email.cc),
    ?assertEqual([], Email#hikyaku_email.bcc),
    ?assertEqual(#{}, Email#hikyaku_email.headers).

from_binary_test() ->
    Email = hikyaku_email:from(hikyaku_email:new(), <<"alice@example.com">>),
    ?assertEqual(
        #hikyaku_address{name = undefined, address = <<"alice@example.com">>},
        Email#hikyaku_email.from
    ).

from_tuple_test() ->
    Email = hikyaku_email:from(hikyaku_email:new(), {<<"Alice">>, <<"alice@example.com">>}),
    ?assertEqual(
        #hikyaku_address{name = <<"Alice">>, address = <<"alice@example.com">>},
        Email#hikyaku_email.from
    ).

from_record_test() ->
    Addr = #hikyaku_address{name = <<"Alice">>, address = <<"alice@example.com">>},
    Email = hikyaku_email:from(hikyaku_email:new(), Addr),
    ?assertEqual(Addr, Email#hikyaku_email.from).

to_appends_test() ->
    E0 = hikyaku_email:new(),
    E1 = hikyaku_email:to(E0, <<"a@x.com">>),
    E2 = hikyaku_email:to(E1, <<"b@x.com">>),
    ?assertEqual(2, length(E2#hikyaku_email.to)),
    ?assertEqual(<<"a@x.com">>, (hd(E2#hikyaku_email.to))#hikyaku_address.address).

cc_appends_test() ->
    E0 = hikyaku_email:new(),
    E1 = hikyaku_email:cc(E0, <<"a@x.com">>),
    E2 = hikyaku_email:cc(E1, <<"b@x.com">>),
    ?assertEqual(2, length(E2#hikyaku_email.cc)).

bcc_appends_test() ->
    E0 = hikyaku_email:new(),
    E1 = hikyaku_email:bcc(E0, <<"a@x.com">>),
    ?assertEqual(1, length(E1#hikyaku_email.bcc)).

reply_to_test() ->
    Email = hikyaku_email:reply_to(hikyaku_email:new(), <<"reply@x.com">>),
    ?assertEqual(<<"reply@x.com">>, (Email#hikyaku_email.reply_to)#hikyaku_address.address).

subject_test() ->
    Email = hikyaku_email:subject(hikyaku_email:new(), <<"Hello">>),
    ?assertEqual(<<"Hello">>, Email#hikyaku_email.subject).

text_body_test() ->
    Email = hikyaku_email:text_body(hikyaku_email:new(), <<"Hi">>),
    ?assertEqual(<<"Hi">>, Email#hikyaku_email.text_body).

html_body_test() ->
    Email = hikyaku_email:html_body(hikyaku_email:new(), <<"<b>Hi</b>">>),
    ?assertEqual(<<"<b>Hi</b>">>, Email#hikyaku_email.html_body).

header_test() ->
    Email = hikyaku_email:header(hikyaku_email:new(), <<"X-Custom">>, <<"value">>),
    ?assertEqual(#{<<"X-Custom">> => <<"value">>}, Email#hikyaku_email.headers).

provider_option_test() ->
    Email = hikyaku_email:provider_option(hikyaku_email:new(), tracking, false),
    ?assertEqual(#{tracking => false}, Email#hikyaku_email.provider_options).

put_to_replaces_test() ->
    E0 = hikyaku_email:to(hikyaku_email:new(), <<"old@x.com">>),
    E1 = hikyaku_email:put_to(E0, [<<"a@x.com">>, <<"b@x.com">>]),
    ?assertEqual(2, length(E1#hikyaku_email.to)),
    ?assertEqual(<<"a@x.com">>, (hd(E1#hikyaku_email.to))#hikyaku_address.address).

put_cc_replaces_test() ->
    E0 = hikyaku_email:cc(hikyaku_email:new(), <<"old@x.com">>),
    E1 = hikyaku_email:put_cc(E0, [<<"new@x.com">>]),
    ?assertEqual(1, length(E1#hikyaku_email.cc)),
    ?assertEqual(<<"new@x.com">>, (hd(E1#hikyaku_email.cc))#hikyaku_address.address).

put_bcc_replaces_test() ->
    E0 = hikyaku_email:bcc(hikyaku_email:new(), <<"old@x.com">>),
    E1 = hikyaku_email:put_bcc(E0, [<<"new@x.com">>]),
    ?assertEqual(1, length(E1#hikyaku_email.bcc)),
    ?assertEqual(<<"new@x.com">>, (hd(E1#hikyaku_email.bcc))#hikyaku_address.address).

attachment_test() ->
    Att = #hikyaku_attachment{
        filename = <<"file.txt">>,
        content_type = <<"text/plain">>,
        data = <<"hello">>
    },
    Email = hikyaku_email:attachment(hikyaku_email:new(), Att),
    ?assertEqual(1, length(Email#hikyaku_email.attachments)),
    ?assertEqual(Att, hd(Email#hikyaku_email.attachments)).

full_pipeline_test() ->
    Email =
        hikyaku_email:text_body(
            hikyaku_email:subject(
                hikyaku_email:to(
                    hikyaku_email:from(
                        hikyaku_email:new(),
                        {<<"Alice">>, <<"alice@example.com">>}
                    ),
                    <<"bob@example.com">>
                ),
                <<"Hello">>
            ),
            <<"Hi Bob">>
        ),
    ?assertEqual(<<"Hello">>, Email#hikyaku_email.subject),
    ?assertEqual(<<"alice@example.com">>, (Email#hikyaku_email.from)#hikyaku_address.address),
    ?assertEqual(1, length(Email#hikyaku_email.to)),
    ?assertEqual(<<"Hi Bob">>, Email#hikyaku_email.text_body).
