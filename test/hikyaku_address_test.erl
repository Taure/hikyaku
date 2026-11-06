-module(hikyaku_address_test).
-include_lib("eunit/include/eunit.hrl").
-include("hikyaku.hrl").

format_without_name_test() ->
    ?assertEqual(~"a@x.com", hikyaku_address:format(#hikyaku_address{address = ~"a@x.com"})).

format_plain_name_unquoted_test() ->
    ?assertEqual(
        ~"Bob Smith <b@x.com>",
        hikyaku_address:format(#hikyaku_address{name = ~"Bob Smith", address = ~"b@x.com"})
    ).

format_quotes_specials_test_() ->
    [
        ?_assertEqual(
            Expected,
            hikyaku_address:format(#hikyaku_address{name = Name, address = ~"b@x.com"})
        )
     || {Name, Expected} <- [
            {~"Bob <ceo@bank.com>, x", ~"\"Bob <ceo@bank.com>, x\" <b@x.com>"},
            {~"Smith, John", ~"\"Smith, John\" <b@x.com>"},
            {~"John Q. Public", ~"\"John Q. Public\" <b@x.com>"},
            {~"Say \"hi\"", ~"\"Say \\\"hi\\\"\" <b@x.com>"},
            {~"back\\slash", ~"\"back\\\\slash\" <b@x.com>"}
        ]
    ].

format_list_test() ->
    ?assertEqual(
        ~"\"A, B\" <a@x.com>, c@x.com",
        hikyaku_address:format_list([
            #hikyaku_address{name = ~"A, B", address = ~"a@x.com"},
            #hikyaku_address{address = ~"c@x.com"}
        ])
    ).
