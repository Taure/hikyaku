-module(hikyaku_adapter_logger_test).
-include_lib("eunit/include/eunit.hrl").
-include("hikyaku.hrl").

deliver_returns_ok_test() ->
    Email = hikyaku_email:text_body(
        hikyaku_email:subject(
            hikyaku_email:to(
                hikyaku_email:from(hikyaku_email:new(), <<"alice@x.com">>),
                <<"bob@x.com">>
            ),
            <<"Test">>
        ),
        <<"body">>
    ),
    {ok, #{status := logged}} = hikyaku_adapter_logger:deliver(Email, #{}).

deliver_custom_level_test() ->
    Email = hikyaku_email:to(
        hikyaku_email:from(hikyaku_email:new(), <<"alice@x.com">>),
        <<"bob@x.com">>
    ),
    {ok, #{status := logged}} = hikyaku_adapter_logger:deliver(Email, #{level => debug}).
