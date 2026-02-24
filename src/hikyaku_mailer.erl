-module(hikyaku_mailer).

-include("hikyaku.hrl").

-export([deliver/2]).

-callback config() -> map().

-spec deliver(module(), #hikyaku_email{}) -> {ok, term()} | {error, term()}.
deliver(Mailer, Email = #hikyaku_email{}) ->
    case validate(Email) of
        ok ->
            Config = Mailer:config(),
            #{adapter := Adapter} = Config,
            Adapter:deliver(Email, Config);
        {error, _} = Err ->
            Err
    end.

%%% Internal

validate(#hikyaku_email{from = undefined}) ->
    {error, {validation, <<"from is required">>}};
validate(#hikyaku_email{to = [], cc = [], bcc = []}) ->
    {error, {validation, <<"at least one recipient (to, cc, or bcc) is required">>}};
validate(#hikyaku_email{}) ->
    ok.
