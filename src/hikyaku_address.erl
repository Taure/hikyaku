-module(hikyaku_address).
-moduledoc false.

-include("hikyaku.hrl").

-export([new/2, format/1, format_list/1]).

%% Builds an address that gen_smtp can parse back as exactly one mailbox,
%% so delivery never fails on a malformed address. An empty name counts as
%% no name.
-spec new(binary() | undefined, binary()) -> #hikyaku_address{}.
new(<<>>, Address) ->
    new(undefined, Address);
new(Name, Address) ->
    case parses_as(Address, Address) andalso is_bare(Address) of
        true -> ok;
        false -> error({invalid_address, address, Address})
    end,
    Addr = #hikyaku_address{name = Name, address = Address},
    case
        Name =:= undefined orelse
            (re:run(Name, ~"[\\x00-\\x1f\\x7f]") =:= nomatch andalso
                parses_as(format(Addr), Address))
    of
        true -> Addr;
        false -> error({invalid_address, name, Name})
    end.

parses_as(Mailbox, Address) ->
    case smtp_util:parse_rfc5322_addresses(Mailbox) of
        {ok, [{_Name, Parsed}]} when is_list(Parsed) ->
            unicode:characters_to_binary(Parsed) =:= Address;
        _ ->
            false
    end.

is_bare(Address) ->
    case smtp_util:parse_rfc5322_addresses(Address) of
        {ok, [{undefined, _}]} -> true;
        _ -> false
    end.

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
