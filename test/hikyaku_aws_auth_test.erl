-module(hikyaku_aws_auth_test).
-include_lib("eunit/include/eunit.hrl").

sign_request_basic_test() ->
    Opts = #{
        method => <<"GET">>,
        url => <<"https://example.amazonaws.com/">>,
        headers => [],
        body => <<>>,
        region => <<"us-east-1">>,
        service => <<"service">>,
        access_key => <<"AKIDEXAMPLE">>,
        secret_key => <<"wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY">>,
        datetime => {{2015, 8, 30}, {12, 36, 0}}
    },
    SignedHeaders = hikyaku_aws_auth:sign_request(Opts),
    AuthHeader = proplists:get_value(<<"authorization">>, SignedHeaders),
    ?assertMatch(<<"AWS4-HMAC-SHA256 ", _/binary>>, AuthHeader),
    ?assertNotEqual(
        nomatch,
        binary:match(
            AuthHeader, <<"Credential=AKIDEXAMPLE/20150830/us-east-1/service/aws4_request">>
        )
    ),
    ?assertNotEqual(nomatch, binary:match(AuthHeader, <<"SignedHeaders=">>)),
    ?assertNotEqual(nomatch, binary:match(AuthHeader, <<"Signature=">>)).

sign_request_with_body_test() ->
    Opts = #{
        method => <<"POST">>,
        url => <<"https://email.us-east-1.amazonaws.com/v2/email/outbound-emails">>,
        headers => [{<<"content-type">>, <<"application/json">>}],
        body => <<"{\"test\":true}">>,
        region => <<"us-east-1">>,
        service => <<"ses">>,
        access_key => <<"AKIAIOSFODNN7EXAMPLE">>,
        secret_key => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
        datetime => {{2024, 1, 15}, {12, 0, 0}}
    },
    SignedHeaders = hikyaku_aws_auth:sign_request(Opts),
    AuthHeader = proplists:get_value(<<"authorization">>, SignedHeaders),
    ?assertNotEqual(nomatch, binary:match(AuthHeader, <<"ses">>)),
    ?assertNotEqual(nomatch, binary:match(AuthHeader, <<"20240115">>)),
    AmzDate = proplists:get_value(<<"x-amz-date">>, SignedHeaders),
    ?assertEqual(<<"20240115T120000Z">>, AmzDate),
    ContentSha = proplists:get_value(<<"x-amz-content-sha256">>, SignedHeaders),
    ?assert(is_binary(ContentSha)),
    ?assertEqual(64, byte_size(ContentSha)).

deterministic_signature_test() ->
    Opts = #{
        method => <<"POST">>,
        url => <<"https://email.us-east-1.amazonaws.com/v2/email/outbound-emails">>,
        headers => [{<<"content-type">>, <<"application/json">>}],
        body => <<"{}">>,
        region => <<"us-east-1">>,
        service => <<"ses">>,
        access_key => <<"AKID">>,
        secret_key => <<"SECRET">>,
        datetime => {{2024, 6, 1}, {0, 0, 0}}
    },
    Headers1 = hikyaku_aws_auth:sign_request(Opts),
    Headers2 = hikyaku_aws_auth:sign_request(Opts),
    Auth1 = proplists:get_value(<<"authorization">>, Headers1),
    Auth2 = proplists:get_value(<<"authorization">>, Headers2),
    ?assertEqual(Auth1, Auth2).

includes_required_headers_test() ->
    Opts = #{
        method => <<"POST">>,
        url => <<"https://ses.us-west-2.amazonaws.com/test">>,
        headers => [],
        body => <<"body">>,
        region => <<"us-west-2">>,
        service => <<"ses">>,
        access_key => <<"AK">>,
        secret_key => <<"SK">>,
        datetime => {{2024, 3, 1}, {10, 30, 0}}
    },
    SignedHeaders = hikyaku_aws_auth:sign_request(Opts),
    ?assertNotEqual(false, proplists:get_value(<<"host">>, SignedHeaders)),
    ?assertNotEqual(false, proplists:get_value(<<"x-amz-date">>, SignedHeaders)),
    ?assertNotEqual(false, proplists:get_value(<<"x-amz-content-sha256">>, SignedHeaders)),
    ?assertNotEqual(false, proplists:get_value(<<"authorization">>, SignedHeaders)).
