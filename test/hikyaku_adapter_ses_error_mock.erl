-module(hikyaku_adapter_ses_error_mock).

-export([post/3]).

post(_Url, _Headers, _Body) ->
    {ok, 400, <<"{\"message\":\"ValidationException\"}">>}.
