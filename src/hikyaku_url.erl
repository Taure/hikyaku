-module(hikyaku_url).

-export([encode/1]).

-spec encode(binary()) -> binary().
encode(Bin) when is_binary(Bin) ->
    encode(Bin, <<>>).

encode(<<>>, Acc) ->
    Acc;
encode(<<C, Rest/binary>>, Acc) when
    (C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse
        C =:= $- orelse C =:= $_ orelse C =:= $. orelse C =:= $~
->
    encode(Rest, <<Acc/binary, C>>);
encode(<<C, Rest/binary>>, Acc) ->
    Hi = hex_char(C bsr 4),
    Lo = hex_char(C band 16#0F),
    encode(Rest, <<Acc/binary, $%, Hi, Lo>>).

hex_char(N) when N < 10 -> N + $0;
hex_char(N) -> N - 10 + $A.
