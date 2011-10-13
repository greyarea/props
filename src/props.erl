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
    F = props_path_parser:parse(Path),
    case F(Props) of
        undefined ->
            Default;
        Result ->
            Result
    end.

%% @doc Set a property in a props structure by path.
-spec set(prop_path(), prop_value(), props()) -> props().
set(Path, Value, Props) ->
    PathList = path_to_list(Path),
    do_set(PathList, Value, Props).

%% @doc Internal naive recursive setter.
-spec do_set([string()], prop_value(), props()) -> props().
do_set([Final], Value, {PropList}) ->
    Key = list_to_binary(Final),
    PropList2 = lists:keystore(Key, 1, PropList, {Key, Value}),
    {PropList2};
do_set([First | Rest] = PathList, Value, {PropList} = Props) ->
    Key = list_to_binary(First),
    
    case props:get(First, Props) of
        {_} = P ->
            PropList2 = lists:keystore(Key, 1, PropList,
                                       {Key, do_set(Rest, Value, P)}),
            {PropList2};
        _ ->
            throw({error, {invalid_path, list_to_path(PathList), Props}})
    end.

%% @doc Return a list of differences between two property structures.
-spec diff(props(), props()) -> [{prop_path(), {prop_value(), prop_value()}}].
diff(_Props1, _Props2) ->
    [].

%% internal functions

%% @doc Convert a path into a list of path parts.
-spec path_to_list(prop_path()) -> [string()].
path_to_list(Path) when is_atom(Path) ->
    path_to_list(atom_to_list(Path));
path_to_list(Path) ->
    string:tokens(Path, ".").

%% @doc Convert a list of path parts into a path.
-spec list_to_path([string()]) -> prop_path().
list_to_path(PathList) ->
    string:join(PathList, ".").
