%% @doc props tests.
-module(props_SUITE).

-export([all/0,
         basic_get_with_atom_path/1,
         array_get_with_atom_path/1,
         basic_get_with_string_path/1,
         array_get_with_string_path/1,
         simple_get/1,
         simple_set/1,
         multi_set/1,
         array_index_change/1,
         create_implicit_props/1,
         create_implicit_array/1]).

-include_lib("common_test/include/ct.hrl").

-define(DATA, {[{<<"a">>, 1},
                {<<"b">>, [2, {[{<<"c">>, 3}]}]},
                {<<"d">>, {[{<<"e">>, {[{<<"f">>, 4}]}}]}}]}).

all() ->
    [basic_get_with_atom_path,
     array_get_with_atom_path,
     basic_get_with_string_path,
     array_get_with_string_path,
     simple_get,
     simple_set,
     multi_set,
     array_index_change,
     create_implicit_props,
     create_implicit_array].

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

simple_set(_Config) ->
    {[{<<"a">>, 1}]} = props:set(a, 1, props:new()).

multi_set(_Config) ->
    Src = {[{<<"a">>, {[{<<"b">>, {[]}}]}},
            {<<"b">>, 2}]},
    Dst = {[{<<"a">>, {[{<<"b">>, {[{<<"c">>, 1}]}}]}},
            {<<"b">>, 2}]},
    Dst = props:set(a.b.c, 1, Src).

array_index_change(_Config) ->
    Src = {[{<<"a">>, [1]}]},
    Dst = {[{<<"a">>, [2]}]},
    Dst = props:set("a[1]", 2, Src).

create_implicit_props(_Config) ->
    Src = {[]},
    Dst = {[{<<"a">>, {[{<<"b">>, 1}]}}]},
    Dst = props:set(a.b, 1, Src).

create_implicit_array(_Config) ->
    Src = {[]},
    Dst = {[{<<"a">>, [1]}]},
    Dst = props:set("a[1]", 1, Src).
