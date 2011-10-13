%% @doc Property structure library
-module(props).

-export([new/0,
         get/2,
         get/3,
         set/3,
         diff/2]).

-export_type([prop_value/0, props/0, prop_path/0]).

-type prop_value() :: true | false | number() | 
                      [prop_value() | props()] | props().
-opaque props() :: {[{binary(), prop_value()}]}.
-type prop_path() :: atom() | string().
-type path_tokens() :: [{prop | index, pos_integer() | binary()}].

-define(INVALID_ACCESS_IDX(Idx, Obj),
        {error, {invalid_access, index, Idx, Obj}}).
-define(INVALID_ACCESS_KEY(Key, Obj),
        {error, {inavlid_access, key, list_to_atom(binary_to_list(Key)), Obj}}).

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

%% @doc Set a property in a props structure by path.
-spec set(prop_path(), prop_value(), props()) -> props().
set(Path, Value, Props) when is_atom(Path) ->
    set(atom_to_list(Path), Value, Props);
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
                        
%% @doc Return a list of differences between two property structures.
-spec diff(props(), props()) -> [{prop_path(), {prop_value(), prop_value()}}].
diff(_Props1, _Props2) ->
    [].
