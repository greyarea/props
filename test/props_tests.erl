%% @doc props tests.
-module(props_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DATA, {[{<<"a">>, 1},
                {<<"b">>, [2, {[{<<"c">>, 3}]}]},
                {<<"d">>, {[{<<"e">>, {[{<<"f">>, 4}]}}]}}]}).

%% Basic get tests.

basic_get_with_atom_path_test() ->
    ?assertEqual(4, props:get(d.e.f, ?DATA)).

array_get_with_atom_path_test() ->
    ?assertEqual(3, props:get('b[2].c', ?DATA)).

basic_get_with_string_path_test() ->
    ?assertEqual(4, props:get("d.e.f", ?DATA)).

array_get_with_string_path_test() ->
    ?assertEqual(3, props:get("b[2].c", ?DATA)).

simple_get_test() ->
    ?assertEqual(1, props:get(a, ?DATA)).
