-module(hikyaku_app_test).
-include_lib("eunit/include/eunit.hrl").

gen_smtp_is_a_runtime_dependency_test() ->
    ok = application:load(hikyaku),
    {ok, Apps} = application:get_key(hikyaku, applications),
    ?assert(lists:member(gen_smtp, Apps)).
