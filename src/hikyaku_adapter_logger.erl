-module(hikyaku_adapter_logger).
-behaviour(hikyaku_adapter).

-include("hikyaku.hrl").

-export([deliver/2]).

-spec deliver(#hikyaku_email{}, map()) -> {ok, #{status := logged}}.
deliver(Email, Config) ->
    Level = maps:get(level, Config, notice),
    logger:log(Level, format_email(Email)),
    {ok, #{status => logged}}.

%%% Internal

format_email(#hikyaku_email{
    subject = Subject,
    from = From,
    to = To,
    cc = Cc,
    bcc = Bcc,
    text_body = Text,
    html_body = Html
}) ->
    #{
        msg => <<"hikyaku_email_delivered">>,
        subject => Subject,
        from => format_address(From),
        to => [format_address(A) || A <- To],
        cc => [format_address(A) || A <- Cc],
        bcc => [format_address(A) || A <- Bcc],
        text_body => Text,
        html_body => Html
    }.

format_address(undefined) ->
    undefined;
format_address(#hikyaku_address{name = undefined, address = Addr}) ->
    Addr;
format_address(#hikyaku_address{name = Name, address = Addr}) ->
    <<Name/binary, " <", Addr/binary, ">">>.
