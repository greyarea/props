%% @doc Property structure library
-module(props).

-export([new/0,
         get/2,
         get/3,
         set/1,
         set/2,
         set/3,
         make/1,
         take/2,
         drop/2,
         merge/2,
         diff/2,
         keys/1,
         fold/3,
         select_matches/2,
	     delete_matches/2,
         to_pretty/1,
         to_string/1,
         to_proplist/1
         ]).

-export_type([prop_value/0, props/0, prop_path/0]).

-type prop_value() :: true | false | number() | 
                      [prop_value() | props()] | props().
-opaque props() :: {[{binary(), prop_value()}]}.
-type prop_path() :: atom() | string() | binary().
-type path_tokens() :: [{prop | index, pos_integer() | binary()}].

-define(INVALID_ACCESS_IDX(Idx, Obj),
        {error, {invalid_access, index, Idx, Obj}}).
-define(INVALID_ACCESS_KEY(Key, Obj),
        {error, {invalid_access, key, list_to_atom(binary_to_list(Key)), Obj}}).

%% @doc Create a new props structure.
-spec new() -> props().
new() ->
    {[]}.

%% @doc Get a property for a props structure.
%% This is equivalent to `get(PathSpec, Props, undefined)`.
-spec get(prop_path(), props()) -> undefined | prop_value().
get(Path, Props) ->
    get(Path, Props, undefined).

%% @doc Get a property in props structure by path.
-spec get(prop_path(), props(), undefined | prop_value()) -> undefined | prop_value().
get(Path, Props, Default) when is_atom(Path) ->
    get(atom_to_list(Path), Props, Default);
get(Path, Props, Default) when is_binary(Path) ->
    do_get([{prop, Path}], Props, Default);
get(Path, Props, Default) ->
    PathTokens = props_path_parser:parse(Path),
    do_get(PathTokens, Props, Default).

%% @doc Internal getter which operates on path tokens.
-spec do_get(path_tokens(), prop_value(), undefined | prop_value()) -> undefined | prop_value().
do_get([], Value, _Default) ->
    Value;
do_get([{prop, Key} | Rest], {PropList}, Default) ->
    case proplists:get_value(Key, PropList, Default) of
        Default ->
            Default;
        Other ->
            do_get(Rest, Other, Default)
    end;
do_get([{prop, Key} | _Rest], NonProps, _Default) ->
    throw(?INVALID_ACCESS_KEY(Key, NonProps));
do_get([{index, Idx} | Rest], List, Default) when is_list(List) ->
    Val = try
              lists:nth(Idx, List)
          catch
              _:_ ->
                  Default
          end,
    case Val of
        Default ->
            Default;
        _ ->
            do_get(Rest, Val, Default)
    end;
do_get([{index, Idx} | _Rest], NonList, _Default) ->
    throw(?INVALID_ACCESS_IDX(Idx, NonList)).

%% @doc Set properties in a new props structure.
-spec set([{prop_path(), prop_value()}]) -> props().
set(PropsList) ->
    set(PropsList, props:new()).

%% @doc Set one or properties in a props structure.
%%
%% With the (Path, Value) form it sets a single property in a blank
%% props structure.
%%
%% With the (List, Props) form it sets all the properties in the given
%% list in the given props structure.
-spec set(prop_path() | [{prop_path(), prop_value()}], prop_value() | props()) -> props().
set([{_, _} | _] = PropsList, Props) ->
    lists:foldl(
      fun({Path, Value}, Acc) ->
              props:set(Path, Value, Acc)
      end, Props, PropsList);
set(Path, Value) ->
    props:set(Path, Value, props:new()).

%% @doc Set a property in a props structure by path.
-spec set(prop_path(), prop_value(), props()) -> props().
set(Path, Value, Props) when is_atom(Path) ->
    set(atom_to_list(Path), Value, Props);
set(Path, Value, Props) when is_binary(Path) ->
    do_set([{prop, Path}], Value, Props);
set(Path, Value, Props) ->
    PathTokens = props_path_parser:parse(Path),
    do_set(PathTokens, Value, Props).

%% @doc Internal naive recursive setter.
-spec do_set(path_tokens(), prop_value(), props()) -> props().
do_set([{prop, Key}], Value, {PropList}) ->
    PropList2 = lists:keystore(Key, 1, PropList, {Key, Value}),
    {PropList2};
do_set([{prop, Key}], _Value, NonProps) ->
    throw(?INVALID_ACCESS_KEY(Key, NonProps));
do_set([{index, Idx}], Value, List) when is_list(List) ->
    try
        {Prefix, Suffix} = case {Idx, List} of
                               {1, []} ->
                                   {[], []};
                               {1, [_ | Suf]} ->
                                   {[], Suf};
                               _ ->
                                   lists:split(Idx - 1, List)
                           end,
        case Suffix of
            [] ->
                lists:append(Prefix, [Value]);
            [_ | Rest] ->
                lists:append(Prefix, [Value], Rest)
        end
    catch
        _:_ ->
            throw(?INVALID_ACCESS_IDX(Idx, List))
    end;
do_set([{index, Idx}], _Value, NonList) ->
    throw(?INVALID_ACCESS_IDX(Idx, NonList));
do_set([{prop, Key} | Rest], Value, {PropList}) ->
    Val = case proplists:get_value(Key, PropList) of
              undefined ->
                  case Rest of
                      [{prop, _} | _] ->
                          do_set(Rest, Value, {[]});
                      [{index, _} | _] ->
                          do_set(Rest, Value, [])
                  end;
              Other ->
                  do_set(Rest, Value, Other)
          end,
    PropList2 = lists:keystore(Key, 1, PropList, {Key, Val}),
    {PropList2};
do_set([{prop, Key} | _Rest], _Value, NonProps) ->
    throw(?INVALID_ACCESS_KEY(Key, NonProps));
do_set([{index, Idx} | Rest], Value, List) when is_list(List) ->
    {Prefix, Suffix} = try
                           lists:split(Idx - 1, List)
                       catch
                           _:_ ->
                               throw(?INVALID_ACCESS_IDX(Idx, List))
                       end,
    case {Suffix, Rest} of
        {[], [{prop, _} | _]} ->
            lists:append(Prefix, [{[]}]);
        {[], [{index, _} | _]} ->
            lists:append(Prefix, [[]]);
        {[OldVal | End], _} ->
            Val = do_set(Rest, Value, OldVal),
            lists:append(Prefix, [Val], End)
    end;
do_set([{index, Idx} | _Rest], _Value, NonList) ->
    throw(?INVALID_ACCESS_IDX(Idx, NonList)).
                        
%% @doc Make a property structure from a proplist.
-spec make(proplists:proplist()) -> props().
make(PropList) ->
    PropList2 = lists:map(
                  fun(Atom) when is_atom(Atom) ->
                          {atom_to_binary(Atom, utf8), true};
                     ({Key, Val}) when is_atom(Key) ->
                          {atom_to_binary(Key, utf8), Val};
                     ({Key, Val}) when is_list(Key) ->
                          {list_to_binary(Key), Val};
                     ({Key, Val}) when is_binary(Key) ->
                          {Key, Val}
                  end, PropList),
    {PropList2}.

%% @doc Convert a mixed atom/binary key list to binary only.
-spec keys_to_binary([atom() | binary()]) -> [binary()].
keys_to_binary(Keys) ->
    lists:map(
      fun(K) ->
              if
                  is_atom(K) ->
                      atom_to_binary(K, utf8);
                  true ->
                      K
              end
      end, Keys).

%% @doc Return a new property structure containing specific keys only.
-spec take([atom() | binary()], props()) -> props().
take(Keys, {PropList}) ->
    BinKeys = keys_to_binary(Keys),
    {lists:filter(
       fun({Key, _Val}) ->
               lists:member(Key, BinKeys)
       end, PropList)}.

%% @doc Return a new property structure without the given keys.
-spec drop([atom() | binary()], props()) -> props().
drop(Keys, {PropList}) ->
    BinKeys = keys_to_binary(Keys),
    {lists:filter(
       fun({Key, _Val}) ->
               not lists:member(Key, BinKeys)
       end, PropList)}.

%% @doc Merge two property structures.
%% Duplicate keys in the second structure overwrite those in the first.
-spec merge(props(), props()) -> props().
merge(Props, {[]}) ->
    Props;
merge({PropList1}, {[{Key, _Val} = Prop | PropList2]}) ->
    NewPropList = lists:keystore(Key, 1, PropList1, Prop),
    merge({NewPropList}, {PropList2}).

%% @doc Return a list of differences between two property structures.
-spec diff(props(), props()) -> [{prop_path(), {prop_value(), prop_value()}}].
diff(_Props1, _Props2) ->
    [].

%% @doc Return the immediate keys in a property structure.
-spec keys(props()) -> [binary()].
keys({PropList}) ->
    proplists:get_keys(PropList).

%% @doc Fold over the immediate keys/vals in a proplist.
-spec fold(fun((binary(), prop_value(), term()) -> term()), term(), props()) -> term().
fold(F, Init, {PropList}) ->
    lists:foldl(
      fun({Key, Val}, Acc) ->
              F(Key, Val, Acc)
      end, Init, PropList).

%% @doc Select props from a list that match certain props.
-spec select_matches([props()], props()) -> [props:props()].
select_matches(PropsList, Props) ->
    select_or_delete_matches(PropsList, Props, select).

%% @doc Delete props from a list that match certain props.
-spec delete_matches([props()], props()) -> [props:props()].
delete_matches(PropsList, Props) ->
    select_or_delete_matches(PropsList, Props, delete).

%% @doc Returns a pretty printed string of the message.
-spec to_pretty(props:props()) -> string().
to_pretty(Props) ->
    do_to_pretty(Props, 0).

%% @doc Converts message term to a string.
-spec term_to_pretty(prop_value(), pos_integer()) -> string().
term_to_pretty({_} = Props, Depth) ->
    do_to_pretty(Props, Depth + 1);
term_to_pretty(Term, _) when is_binary(Term) ->
    io_lib:format("\"~ts\"", [Term]);
term_to_pretty([_|_] = List, Depth) ->
    do_to_pretty(List, Depth + 1);
term_to_pretty(Term, _) ->
    io_lib:format("~p", [Term]).


%% @doc Converts message properties to a string.
-spec do_to_pretty(props(), pos_integer()) -> string().
do_to_pretty({[]}, _) ->
    "{}";
do_to_pretty({PropList}, Depth) ->
    Indent = string:chars($ , Depth * 4),
    F = fun ({Key, Value}, Acc) ->
		KeyStr = io_lib:format("~ts", [Key]),
		ValueStr = term_to_pretty(Value, Depth),
		KeyIndent = string:chars($ , (Depth + 1) * 4),
		[$\n, $,, ValueStr, $ , $:, KeyIndent ++ KeyStr | Acc]
	end,
    [$\n, $, | Acc1] = lists:foldl(F, "\n{", PropList),
    lists:flatten(lists:reverse([Indent ++ "}", $\n | Acc1]));
do_to_pretty([], _) ->
    "[]";
do_to_pretty([_|_] = List, Depth) ->
    Indent = string:chars($ , Depth * 4),
    F = fun(Value, Acc) ->
                ValueStr = term_to_pretty(Value, Depth),
                ValueIndent = string:chars($ , (Depth + 1) * 4),
                [$\n, $,, ValueStr, ValueIndent | Acc]
        end,
    [$\n, $, | Acc1] = lists:foldl(F, "\n[", List),
    lists:flatten(lists:reverse([Indent ++ "]", $\n | Acc1])).

%% @doc Returns a printed string of the property structure, similar to JSON.
-spec to_string(props:props()) -> string().
to_string({[]}) ->
    "{}";
to_string([]) ->
    "[]";
to_string({PropList}) ->
    S = lists:foldl(
          fun({Key, Val}, Acc) ->
                  KeyStr = io_lib:format("~ts", [Key]),
                  ValStr = term_to_string(Val),
                  [$ , $,, ValStr, $ , $:, KeyStr | Acc]
          end, "{", PropList),
    [$ , $, | S2] = S,
    lists:flatten(lists:reverse([$} | S2]));
to_string(List) when is_list(List) ->
    S = lists:foldl(
          fun(Val, Acc) ->
                  ValStr = term_to_string(Val),
                  [$ , $,, ValStr | Acc]
          end, "[", List),
    [$ , $, | S2] = S,
    lists:flatten(lists:reverse([$] | S2])).

%% @doc Stringify a term.
-spec term_to_string(prop_value()) -> string().
term_to_string({_} = Props) ->
    to_string(Props);
term_to_string(List) when is_list(List) ->
    to_string(List);
term_to_string(Binary) when is_binary(Binary) ->
    lists:flatten(io_lib:format("\"~ts\"", [Binary]));
term_to_string(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).


%% @doc Returns a proplist representation
-spec to_proplist(props:props()) -> proplists:proplist().
to_proplist({PropList}) when is_list(PropList) ->
    to_proplist(PropList);
to_proplist(PropList) when is_list(PropList) -> 
    [{K, to_proplist(V)} || {K, V} <- PropList] ++ [ to_proplist(Props) || {Props} <- PropList];
to_proplist(Value) ->
    Value.

%% Internal functions

%% @doc Select or delete props from a list of props.
-spec select_or_delete_matches([props()], props(), select | delete) -> [props()].
select_or_delete_matches(PropsList, {MatchProps}, SelectOrDelete) ->
    FilterMatch =
	fun(Props) ->
		lists:takewhile(
		  fun({MPKey, MPValue}) ->
			  case props:get(MPKey, Props) of
			      Value when SelectOrDelete =:= select,
					 Value =:= MPValue -> 
				  true;
			      Value when SelectOrDelete =:= delete,
					 Value =/= MPValue ->
				  true;
			      _ ->
				  false
			  end
		  end, MatchProps)
	end,
    lists:filter(
      fun(Props) ->
	      case FilterMatch(Props) of
		  Matched when Matched =:= MatchProps->
		      true;
		  _ ->
		      false
	      end
      end, PropsList).
