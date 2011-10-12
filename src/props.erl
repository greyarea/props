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
set(_Path, _Value, Props) ->
    Props.

%% @doc Return a list of differences between two property structures.
-spec diff(props(), props()) -> [{prop_path(), {prop_value(), prop_value()}}].
diff(_Props1, _Props2) ->
    [].
