-module(hikyaku_http).

-export([post/4]).

-callback post(binary(), [{binary(), binary()}], binary()) ->
    {ok, pos_integer(), binary()} | {error, term()}.

-spec post(module() | default, binary(), [{binary(), binary()}], binary()) ->
    {ok, pos_integer(), binary()} | {error, term()}.
post(default, Url, Headers, Body) ->
    httpc_post(Url, Headers, Body);
post(Mod, Url, Headers, Body) ->
    Mod:post(Url, Headers, Body).

%%% Internal

httpc_post(Url, Headers, Body) ->
    HttpHeaders = [
        {binary_to_list(K), binary_to_list(V)}
     || {K, V} <- Headers, K =/= <<"content-type">>
    ],
    ContentType =
        case lists:keyfind(<<"content-type">>, 1, Headers) of
            {_, CT} -> binary_to_list(CT);
            false -> "application/json"
        end,
    Request = {binary_to_list(Url), HttpHeaders, ContentType, Body},
    case httpc:request(post, Request, [{ssl, ssl_opts()}], [{body_format, binary}]) of
        {ok, {{_, StatusCode, _}, _RespHeaders, RespBody}} ->
            {ok, StatusCode, RespBody};
        {error, Reason} ->
            {error, Reason}
    end.

ssl_opts() ->
    [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}].
