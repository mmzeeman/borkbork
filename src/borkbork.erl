%%
%%
%%

-module(borkbork).

-export([parse/1]).

parse(Filename) ->
    case file:read_file(Filename) of
        {ok, Data} ->
            Tokens = b_scanner:scan(Data),
            b_parser:parse(Tokens)
    end.
