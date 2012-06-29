%% props_path_parser.erl
%%
%% Copyright 2011-2012 Grey Area
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%
%% Very similar to JSON paths in JavaScript:
%%
%% Example:
%%   foo.bar[2].baz
%%
%% Paths can be either a string or an atom so both
%%   foo.bar
%% and
%%   "foo.bar"
%% will work the same.
%%
%% Indexes are 1-based, just like Erlang's lists module.

-module(props_path_parser).
-export([parse/1]).

-spec parse(binary() | list()) -> [{prop, binary()} | {index, integer()}].
parse(Path) when is_binary(Path) -> parse(binary_to_list(Path));
parse(Path) ->
    Tokens = string:tokens(Path, "."),
    Parsed = lists:map(fun(Token) ->
			       case string:tokens(Token, "[]") of
				   [P, I] ->
				       {Index, _} = string:to_integer(I),
				       [{prop, list_to_binary(P)}, {index, Index}];
				   [P] ->
				       [{prop, list_to_binary(P)}]
			       end
		       end, Tokens),
    lists:flatten(Parsed).
