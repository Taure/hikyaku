-module(hikyaku_adapter).

-include("hikyaku.hrl").

-type config() :: map().

-export_type([config/0]).

-callback deliver(#hikyaku_email{}, config()) ->
    {ok, term()} | {error, term()}.
