-module(hikyaku_adapter_resend_error_mock).

-export([post/3]).

post(_Url, _Headers, _Body) ->
    {ok, 422,
        <<"{\"statusCode\":422,\"name\":\"validation_error\",\"message\":\"Invalid `to` field.\"}">>}.
