-module(hikyaku_address).
-moduledoc false.

-include("hikyaku.hrl").

-export([format/1, format_list/1]).

-spec format(#hikyaku_address{}) -> binary().
format(#hikyaku_address{name = undefined, address = Addr}) ->
    Addr;
format(#hikyaku_address{name = Name, address = Addr}) ->
    <<(display_name(Name))/binary, " <", Addr/binary, ">">>.

-spec format_list([#hikyaku_address{}]) -> binary().
format_list([]) ->
    <<>>;
format_list([First | Rest]) ->
    <<(format(First))/binary, <<<<", ", (format(A))/binary>> || A <- Rest>>/binary>>.

%% RFC 5322 specials would otherwise let a display name such as
%% `Bob <ceo@bank.com>, x` add a second mailbox to the header.
display_name(Name) ->
    case re:run(Name, ~"[()<>\\[\\]:;@\\\\,.\"]") of
        nomatch -> Name;
        {match, _} -> <<$", <<<<(escape(C))/binary>> || <<C>> <= Name>>/binary, $">>
    end.

escape($") -> <<$\\, $">>;
escape($\\) -> <<$\\, $\\>>;
escape(C) -> <<C>>.
