-module(hikyaku_adapter_test).
-behaviour(hikyaku_adapter).

-include("hikyaku.hrl").

-export([deliver/2]).

-spec deliver(#hikyaku_email{}, map()) -> {ok, #{status := sent}}.
deliver(Email, Config) ->
    Pid = maps:get(pid, Config, self()),
    Pid ! {hikyaku_email, Email},
    {ok, #{status => sent}}.
