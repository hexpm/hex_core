%% Vendored from verl v1.0.2, do not edit manually

-module(hex_core_verl_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [compare_test, parse_test, parse_requirement_test, compile_requirement_test, is_match_test].

compare_test(_Cfg) ->
    gt = hex_core_verl:compare(<<"1.0.1">>, <<"1.0.0">>),
    gt = hex_core_verl:compare(<<"1.1.0">>, <<"1.0.1">>),
    gt = hex_core_verl:compare(<<"2.1.1">>, <<"1.2.2">>),
    gt = hex_core_verl:compare(<<"1.0.0">>, <<"1.0.0-dev">>),
    gt = hex_core_verl:compare(<<"1.2.3-dev">>, <<"0.1.2">>),
    gt = hex_core_verl:compare(<<"1.0.0-a.b">>, <<"1.0.0-a">>),
    gt = hex_core_verl:compare(<<"1.0.0-b">>, <<"1.0.0-a.b">>),
    gt = hex_core_verl:compare(<<"1.0.0-a">>, <<"1.0.0-0">>),
    gt = hex_core_verl:compare(<<"1.0.0-a.b">>, <<"1.0.0-a.a">>),
    lt = hex_core_verl:compare(<<"1.0.0">>, <<"1.0.1">>),
    lt = hex_core_verl:compare(<<"1.0.1">>, <<"1.1.0">>),
    lt = hex_core_verl:compare(<<"1.2.2">>, <<"2.1.1">>),
    lt = hex_core_verl:compare(<<"1.0.0-dev">>, <<"1.0.0">>),
    lt = hex_core_verl:compare(<<"0.1.2">>, <<"1.2.3-dev">>),
    lt = hex_core_verl:compare(<<"1.0.0-a">>, <<"1.0.0-a.b">>),
    lt = hex_core_verl:compare(<<"1.0.0-a.b">>, <<"1.0.0-b">>),
    lt = hex_core_verl:compare(<<"1.0.0-0">>, <<"1.0.0-a">>),
    lt = hex_core_verl:compare(<<"1.0.0-a.a">>, <<"1.0.0-a.b">>),
    eq = hex_core_verl:compare(<<"1.0.0">>, <<"1.0.0">>),
    eq = hex_core_verl:compare(<<"1.0.0-dev">>, <<"1.0.0-dev">>),
    eq = hex_core_verl:compare(<<"1.0.0-a">>, <<"1.0.0-a">>),
    {error, invalid_version} = hex_core_verl:compare(<<"1.0">>, <<"1.0.0">>),
    {error, invalid_version} = hex_core_verl:compare(<<"1.0.0-dev">>, <<"1.0">>),
    {error, invalid_version} = hex_core_verl:compare(<<"foo">>, <<"1.0.0-a">>).

parse_test(_Cfg) ->
    Exp0 = #{major => 1, minor => 2, patch => 3, pre => [], build => undefined},
    {ok, Exp0} = hex_core_verl:parse(<<"1.2.3">>),
    Exp1 = #{major => 1, minor => 4, patch => 5, pre => [], build => <<"ignore">>},
    {ok, Exp1} = hex_core_verl:parse(<<"1.4.5+ignore">>),
    Exp2 = #{major => 0, minor => 0, patch => 1, pre => [], build => <<"sha0702245">>},
    {ok, Exp2} = hex_core_verl:parse(<<"0.0.1+sha.0702245">>),
    Exp3 = #{major => 1, minor => 4, patch => 5, pre => [<<"6-g3318bd5">>], build => undefined},
    {ok, Exp3} = hex_core_verl:parse(<<"1.4.5-6-g3318bd5">>),
    Exp4 = #{major => 1, minor => 4, patch => 5, pre => [6, 7, <<"eight">>], build => undefined},
    {ok, Exp4} = hex_core_verl:parse(<<"1.4.5-6.7.eight">>),
    Exp5 = #{major => 1, minor => 4, patch => 5, pre => [<<"6-g3318bd5">>], build => <<"ignore">>},
    {ok, Exp5} = hex_core_verl:parse(<<"1.4.5-6-g3318bd5+ignore">>),
    ExpErr = {error, invalid_version},
    ExpErr = hex_core_verl:parse(<<"foobar">>),
    ExpErr =  hex_core_verl:parse(<<"2">>),
    ExpErr =  hex_core_verl:parse(<<"2.">>),
    ExpErr =  hex_core_verl:parse(<<"2.3">>),
    ExpErr =  hex_core_verl:parse(<<"2.3.">>),
    ExpErr =  hex_core_verl:parse(<<"2.3.0-">>),
    ExpErr =  hex_core_verl:parse(<<"2.3.0+">>),
    ExpErr =  hex_core_verl:parse(<<"2.3.0.">>),
    ExpErr =  hex_core_verl:parse(<<"2.3.0.4">>),
    ExpErr =  hex_core_verl:parse(<<"2.3.-rc.1">>),
    ExpErr =  hex_core_verl:parse(<<"2.3.+rc.1">>),
    ExpErr =  hex_core_verl:parse(<<"2.3.0-01">>),
    ExpErr =  hex_core_verl:parse(<<"2.3.00-1">>),
    ExpErr =  hex_core_verl:parse(<<"2.3.00">>),
    ExpErr =  hex_core_verl:parse(<<"2.03.0">>),
    ExpErr =  hex_core_verl:parse(<<"02.3.0">>).

parse_requirement_test(_Cfg) ->
    Str = <<"1.2.3">>,
    ExpSpec = [{{'$1','$2','$3','$4','$5'},
                [{'==',{{'$1','$2','$3','$4'}},{const,{1,2,3,[]}}}],
                ['$_']}],
    {ok, #{string := Str, matchspec := ExpSpec, compiled := false}} =
    hex_core_verl:parse_requirement(Str),
    ExpErr = {error, invalid_requirement},
    ExpErr = hex_core_verl:parse_requirement(<<"1">>),
    ExpErr = hex_core_verl:parse_requirement(<<"1.2">>),
    ExpErr = hex_core_verl:parse_requirement(<<"1.2-3">>),
    ExpErr = hex_core_verl:parse_requirement(<<"_ 1.2.3">>),
    ExpErr = hex_core_verl:parse_requirement(<<"( ) 1.2.3">>).

compile_requirement_test(_Cfg) ->
    {ok, Req} = hex_core_verl:parse_requirement(<<"1.2.3">>),
    #{compiled := true, matchspec := Ref} = hex_core_verl:compile_requirement(Req),

    {Ver, []} = string:to_integer(erlang:system_info(otp_release)),
    case Ver of
        N when N < 20 ->
            true = is_binary(Ref);
        N when N >= 20 ->
            true = is_reference(Ref)
    end.

is_match_test(_Cfg) ->
    {error, invalid_version} = hex_core_verl:is_match(<<"foo">>, <<"2.3.0">>),
    {error, invalid_requirement} = hex_core_verl:is_match(<<"2.3.0">>, <<"foo">>),
    true = hex_core_verl:is_match(<<"2.3.0">>, <<"== 2.3.0">>),
    true = hex_core_verl:is_match(<<"2.3.0">>, <<"~> 2.3.0">>),
    true = hex_core_verl:is_match(<<"1.2.3-alpha">>, <<"1.2.3-alpha">>),
    true = hex_core_verl:is_match(<<"0.9.3">>, <<"== 0.9.3+dev">>),
    true = hex_core_verl:is_match(<<"2.3.0">>, <<"2.3.0">>),
    false = hex_core_verl:is_match(<<"2.4.0">>, <<"2.3.0">>),
    false = hex_core_verl:is_match(<<"2.3.0">>, <<"!= 2.3.0">>),
    false = hex_core_verl:is_match(<<"2.3.0">>, <<"<= 2.2.0">>),
    {ok, Ver} = hex_core_verl:parse(<<"2.3.0">>),
    {ok, Req} = hex_core_verl:parse_requirement(<<"2.3.0">>),
    {error, invalid_version} = hex_core_verl:is_match(<<"foo">>, Req),
    true = hex_core_verl:is_match(Ver, Req),
    true = hex_core_verl:is_match(<<"2.3.0">>, Req),
    true = hex_core_verl:is_match(Ver, <<"2.3.0">>),
    {error, invalid_requirement} = hex_core_verl:is_match(Ver, <<"= 2.3.0">>),
    true = hex_core_verl:is_match(Ver, Req, []),
    {error, invalid_version} = hex_core_verl:is_match(<<".3.0">>, Req, []),
    true = hex_core_verl:is_match(Ver, <<"== 2.3.0">>, []),
    true = hex_core_verl:is_match(<<"2.3.0">>, Req, []),
    {error, invalid_version} = hex_core_verl:is_match(<<"0">>, <<"== 2.3.0">>, []),
    {error, invalid_requirement} = hex_core_verl:is_match(Ver, <<"= 2.3.0">>, []),
    {error, invalid_requirement} = hex_core_verl:is_match(<<"2.3.0">>, <<"= 2.3.0">>, []),
    true = hex_core_verl:is_match(<<"2.3.0">>, <<"== 2.3.0">>, []),
    Compiled = hex_core_verl:compile_requirement(Req),
    true = hex_core_verl:is_match(Ver, Compiled, []),
    true = hex_core_verl:is_match(<<"2.4.0">>, <<"!2.3.0">>),
    false = hex_core_verl:is_match(<<"2.3.0">>, <<"!2.3.0">>),
    true = hex_core_verl:is_match(<<"2.4.0">>, <<"!= 2.3.0">>),
    false = hex_core_verl:is_match(<<"2.3.0">>, <<"!= 2.3.0">>),
    true = hex_core_verl:is_match(<<"2.4.0">>, <<"> 2.3.0">>),
    false = hex_core_verl:is_match(<<"2.2.0">>, <<"> 2.3.0">>),
    false = hex_core_verl:is_match(<<"2.3.0">>, <<"> 2.3.0">>),

    true = hex_core_verl:is_match(<<"1.2.3">>, <<"> 1.2.3-alpha">>),
    true = hex_core_verl:is_match(<<"1.2.3-alpha.1">>, <<"> 1.2.3-alpha">>),
    true = hex_core_verl:is_match(<<"1.2.3-alpha.beta.sigma">>, <<"> 1.2.3-alpha.beta">>),
    false = hex_core_verl:is_match(<<"1.2.3-alpha.10">>, <<"< 1.2.3-alpha.1">>),
    false = hex_core_verl:is_match(<<"0.10.2-dev">>, <<"> 0.10.2">>),

    true = hex_core_verl:is_match(<<"2.4.0">>, <<">= 2.3.0">>),
    false = hex_core_verl:is_match(<<"2.2.0">>, <<">= 2.3.0">>),
    true = hex_core_verl:is_match(<<"2.3.0">>, <<">= 2.3.0">>),
    true = hex_core_verl:is_match(<<"2.0.0">>, <<">= 1.0.0">>),
    true = hex_core_verl:is_match(<<"1.0.0">>, <<"1.0.0">>),


    true = hex_core_verl:is_match(<<"2.2.0">>, <<"< 2.3.0">>),
    false = hex_core_verl:is_match(<<"2.4.0">>, <<"< 2.3.0">>),
    false = hex_core_verl:is_match(<<"2.3.0">>, <<"< 2.3.0">>),
    true = hex_core_verl:is_match(<<"0.10.2-dev">>, <<"< 0.10.2">>),
    false = hex_core_verl:is_match(<<"1.0.0">>, <<"< 1.0.0-dev">>),
    false = hex_core_verl:is_match(<<"1.2.3-dev">>, <<"< 0.1.2">>),

    true = hex_core_verl:is_match(<<"2.2.0">>, <<"<= 2.3.0">>),
    false = hex_core_verl:is_match(<<"2.4.0">>, <<"<= 2.3.0">>),
    true = hex_core_verl:is_match(<<"2.3.0">>, <<"<= 2.3.0">>),

    true = hex_core_verl:is_match(<<"3.0.0">>, <<"~> 3.0">>),
    true = hex_core_verl:is_match(<<"3.2.0">>, <<"~> 3.0">>),
    false = hex_core_verl:is_match(<<"4.0.0">>, <<"~> 3.0">>),
    false = hex_core_verl:is_match(<<"4.4.0">>, <<"~> 3.0">>),

    true = hex_core_verl:is_match(<<"3.0.2">>, <<"~> 3.0.0">>),
    true = hex_core_verl:is_match(<<"3.0.0">>, <<"~> 3.0.0">>),
    false = hex_core_verl:is_match(<<"3.1.0">>, <<"~> 3.0.0">>),
    false = hex_core_verl:is_match(<<"3.4.0">>, <<"~> 3.0.0">>),

    true =  hex_core_verl:is_match(<<"3.6.0">>, <<"~> 3.5">>),
    true =  hex_core_verl:is_match(<<"3.5.0">>, <<"~> 3.5">>),
    false = hex_core_verl:is_match(<<"4.0.0">>, <<"~> 3.5">>),
    false = hex_core_verl:is_match(<<"5.0.0">>, <<"~> 3.5">>),

    true =  hex_core_verl:is_match(<<"3.5.2">>, <<"~> 3.5.0">>),
    true =  hex_core_verl:is_match(<<"3.5.4">>, <<"~> 3.5.0">>),
    false = hex_core_verl:is_match(<<"3.6.0">>, <<"~> 3.5.0">>),
    false = hex_core_verl:is_match(<<"3.6.3">>, <<"~> 3.5.0">>),

    true =  hex_core_verl:is_match(<<"0.9.3">>, <<"~> 0.9.3-dev">>),
    false = hex_core_verl:is_match(<<"0.10.0">>, <<"~> 0.9.3-dev">>),

    false = hex_core_verl:is_match(<<"0.3.0-dev">>, <<"~> 0.2.0">>),

    false = hex_core_verl:is_match(<<"2.2.0-dev">>, <<"~> 2.1.0">>),
    false = hex_core_verl:is_match(<<"2.2.0-dev">>, <<"~> 2.1.0">>, [{allow_pre,
                                                             false}]),
    false = hex_core_verl:is_match(<<"2.2.0-dev">>, <<"~> 2.1.0-dev">>),
    false = hex_core_verl:is_match(<<"2.2.0-dev">>, <<"~> 2.1.0-dev">>, [{allow_pre, false}]).
