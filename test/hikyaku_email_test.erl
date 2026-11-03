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

accessors_test() ->
    Att = hikyaku_attachment:from_data(~"a.txt", ~"data"),
    E0 = hikyaku_email:from(hikyaku_email:new(), {~"Alice", ~"alice@example.com"}),
    E1 = hikyaku_email:to(hikyaku_email:to(E0, ~"bob@example.com"), ~"carol@example.com"),
    E2 = hikyaku_email:bcc(hikyaku_email:cc(E1, ~"cc@example.com"), ~"bcc@example.com"),
    E3 = hikyaku_email:subject(hikyaku_email:reply_to(E2, ~"reply@example.com"), ~"Subject"),
    E4 = hikyaku_email:html_body(hikyaku_email:text_body(E3, ~"text"), ~"<b>html</b>"),
    E5 = hikyaku_email:header(hikyaku_email:attachment(E4, Att), ~"X-Tag", ~"tag"),
    Email = hikyaku_email:provider_option(E5, tag, ~"welcome"),
    ?assertEqual(~"Subject", hikyaku_email:subject(Email)),
    ?assertEqual(
        #hikyaku_address{name = ~"Alice", address = ~"alice@example.com"},
        hikyaku_email:from(Email)
    ),
    ?assertEqual(
        [~"bob@example.com", ~"carol@example.com"],
        [A#hikyaku_address.address || A <- hikyaku_email:to(Email)]
    ),
    ?assertEqual([#hikyaku_address{address = ~"cc@example.com"}], hikyaku_email:cc(Email)),
    ?assertEqual([#hikyaku_address{address = ~"bcc@example.com"}], hikyaku_email:bcc(Email)),
    ?assertEqual(#hikyaku_address{address = ~"reply@example.com"}, hikyaku_email:reply_to(Email)),
    ?assertEqual(~"text", hikyaku_email:text_body(Email)),
    ?assertEqual(~"<b>html</b>", hikyaku_email:html_body(Email)),
    ?assertEqual([Att], hikyaku_email:attachments(Email)),
    ?assertEqual(#{~"X-Tag" => ~"tag"}, hikyaku_email:headers(Email)),
    ?assertEqual(#{tag => ~"welcome"}, hikyaku_email:provider_options(Email)).

accessors_on_new_email_test() ->
    Email = hikyaku_email:new(),
    ?assertEqual(<<>>, hikyaku_email:subject(Email)),
    ?assertEqual(undefined, hikyaku_email:from(Email)),
    ?assertEqual([], hikyaku_email:to(Email)),
    ?assertEqual(undefined, hikyaku_email:reply_to(Email)),
    ?assertEqual(undefined, hikyaku_email:text_body(Email)),
    ?assertEqual(undefined, hikyaku_email:html_body(Email)),
    ?assertEqual([], hikyaku_email:attachments(Email)),
    ?assertEqual(#{}, hikyaku_email:headers(Email)).

header_rejects_invalid_names_test_() ->
    [
        ?_assertError({invalid_header, Name}, hikyaku_email:header(hikyaku_email:new(), Name, ~"v"))
     || Name <- [~"", ~"X-K: v\r\nX-Inj", ~"X K", ~"X:K", binary:copy(~"x", 77)]
    ].

header_rejects_crlf_in_value_test_() ->
    [
        ?_assertError(
            {invalid_header, ~"X-A"}, hikyaku_email:header(hikyaku_email:new(), ~"X-A", Value)
        )
     || Value <- [~"v\r\nX-Injected: 1", ~"v\nX-Injected: 1", ~"v\r"]
    ].

header_rejects_reserved_names_test_() ->
    [
        ?_assertError(
            {reserved_header, Name}, hikyaku_email:header(hikyaku_email:new(), Name, ~"v")
        )
     || Name <- [
            ~"From",
            ~"to",
            ~"CC",
            ~"Bcc",
            ~"Reply-To",
            ~"Subject",
            ~"Content-Type",
            ~"MIME-Version",
            ~"content-transfer-encoding"
        ]
    ].

header_accepts_message_id_and_list_unsubscribe_test() ->
    E0 = hikyaku_email:header(hikyaku_email:new(), ~"Message-ID", ~"<id@example.com>"),
    E1 = hikyaku_email:header(E0, ~"List-Unsubscribe", ~"<mailto:u@example.com>"),
    ?assertEqual(
        #{~"Message-ID" => ~"<id@example.com>", ~"List-Unsubscribe" => ~"<mailto:u@example.com>"},
        hikyaku_email:headers(E1)
    ).

address_rejects_control_characters_test_() ->
    New = hikyaku_email:new(),
    [
        ?_assertError(
            {invalid_address, name, _}, hikyaku_email:from(New, {~"Bob\r\nX-Name: 1", ~"b@x.com"})
        ),
        ?_assertError(
            {invalid_address, address, _}, hikyaku_email:to(New, ~"c@example.com\r\nX-To: 1")
        ),
        ?_assertError({invalid_address, address, _}, hikyaku_email:cc(New, ~"a@x.com, b@y.com")),
        ?_assertError({invalid_address, address, _}, hikyaku_email:bcc(New, ~"<a@x.com>")),
        ?_assertError(
            {invalid_address, address, _}, hikyaku_email:put_to(New, [~"ok@x.com", ~"a b@x.com"])
        ),
        ?_assertError(
            {invalid_address, address, _},
            hikyaku_email:reply_to(New, #hikyaku_address{address = ~"r@x.com\n"})
        )
    ].

address_rejects_malformed_mailboxes_test_() ->
    [
        ?_assertError(
            {invalid_address, address, Address}, hikyaku_email:to(hikyaku_email:new(), Address)
        )
     || Address <- [
            ~"a@b.com;c@d.com",
            ~"a(x)@b.com",
            ~"localuser",
            ~"Bob <b@x.com>",
            ~"a..b@x.com",
            ~"a@b@c.com",
            ~""
        ]
    ].

address_accepts_valid_mailboxes_test_() ->
    [
        ?_test(begin
            Email = hikyaku_email:from(hikyaku_email:to(hikyaku_email:new(), Address), Address),
            [#hikyaku_address{address = Address}] = hikyaku_email:to(Email),
            Encoded = mimemail:encode(hikyaku_adapter_smtp:build_mimemail(Email)),
            ?assert(is_binary(Encoded))
        end)
     || Address <- [
            ~"bob+tag@example.com",
            ~"bob@bücher.de",
            ~"\"a\"@b.com",
            ~"\"a b\"@b.com",
            ~"user@localhost",
            ~"a@[127.0.0.1]"
        ]
    ].

display_name_accepted_and_encodable_test_() ->
    [
        ?_test(begin
            Email = hikyaku_email:to(
                hikyaku_email:from(hikyaku_email:new(), {Name, ~"a@x.com"}), ~"b@x.com"
            ),
            ?assert(is_binary(mimemail:encode(hikyaku_adapter_smtp:build_mimemail(Email))))
        end)
     || Name <- [
            ~"Bob Smith",
            ~"Bob <ceo@bank.com>, x",
            ~"Bob \"The\" \\ B",
            ~"Åsa Öberg, AB",
            ~"  spaced  "
        ]
    ].

empty_display_name_is_no_name_test() ->
    Email = hikyaku_email:from(hikyaku_email:new(), {<<>>, ~"a@x.com"}),
    ?assertEqual(#hikyaku_address{address = ~"a@x.com"}, hikyaku_email:from(Email)).

header_rejects_control_characters_test_() ->
    [
        ?_assertError(
            {invalid_header, ~"X-A"}, hikyaku_email:header(hikyaku_email:new(), ~"X-A", V)
        )
     || V <- [<<"a", 0, "b">>, <<"a", 27, "b">>, <<"a", 127, "b">>]
    ].

header_accepts_tab_in_value_test() ->
    Email = hikyaku_email:header(hikyaku_email:new(), ~"X-A", ~"a\tb"),
    ?assertEqual(#{~"X-A" => ~"a\tb"}, hikyaku_email:headers(Email)).

header_rejects_provider_reserved_names_test_() ->
    [
        ?_assertError(
            {reserved_header, Name}, hikyaku_email:header(hikyaku_email:new(), Name, ~"v")
        )
     || Name <- [~"X-SG-ID", ~"x-sg-eid", ~"Received", ~"DKIM-Signature"]
    ].

attachment_rejects_invalid_record_test() ->
    Att = (hikyaku_attachment:from_data(~"a.txt", ~"data"))#hikyaku_attachment{
        content_type = ~"text/plain\r\nX-CT: 1"
    },
    ?assertError(
        {invalid_attachment, content_type, _}, hikyaku_email:attachment(hikyaku_email:new(), Att)
    ).
