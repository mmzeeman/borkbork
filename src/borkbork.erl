%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% @author Maas-Maarten Zeeman
%% @copyright (c) 2018 Maas-Maarten Zeeman <me@mmzeeman.nl>

-module(borkbork).

-export([
    parse/1,
    debug_msg/3
]).

-include("borkbork.hrl").

-type ast_node() ::
    #drakon{} | #primitive{} | #silhouette{} | #skewer{} | #branch{} | #question{} | #action{} | #insertion{} | #address{} | #link{} | #connector{}.

-export_type([
    ast_node/0
]).

parse(Filename) ->
    case file:read_file(Filename) of
        {ok, Data} ->
            Tokens = b_scanner:scan(Data),
            AST  = b_parser:parse(Tokens),
            A = AST#drakon.attributes,
            AST#drakon{attributes=maps:put(filename, Filename, A)}
    end.

%% @doc Echo and return a debugging value
debug_msg(Module, Line, Msg) ->
    error_logger:info_msg("DEBUG: ~p:~p  ~p~n", [Module, Line, Msg]),
    Msg.


