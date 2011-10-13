%% @doc props tests.
-module(props_SUITE).

-export([all/0,
         basic_get_with_atom_path/1,
         array_get_with_atom_path/1,
         basic_get_with_string_path/1,
         array_get_with_string_path/1,
         simple_get/1]).

-include_lib("common_test/include/ct.hrl").

-define(DATA, {[{<<"a">>, 1},
                {<<"b">>, [2, {[{<<"c">>, 3}]}]},
                {<<"d">>, {[{<<"e">>, {[{<<"f">>, 4}]}}]}}]}).

all() ->
    [basic_get_with_atom_path,
     array_get_with_atom_path,
     basic_get_with_string_path,
     array_get_with_string_path,
     simple_get].

%% Basic get tests.

basic_get_with_atom_path(_Config) ->
    4 = props:get(d.e.f, ?DATA).

array_get_with_atom_path(_Config) ->
    3 = props:get('b[2].c', ?DATA).

basic_get_with_string_path(_Config) ->
    4 = props:get("d.e.f", ?DATA).

array_get_with_string_path(_Config) ->
    3 = props:get("b[2].c", ?DATA).

simple_get(_Config) ->
    1 = props:get(a, ?DATA).
