%%
%%
%%

-module(b_scanner).

-export([
    scan/1
]).

-record(state, {
    position = 1
}).

-define(IS_WHITESPACE(C),
    (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

-define(IS_SEPARATOR(C),
    (C =:= $( orelse C =:= $) orelse C =:= ${ orelse C =:= $} orelse C =:= $,)).

% scan(iolist()) -> borkbork:tokens()
scan(Data) ->
    scan(iolist_to_binary(Data), #state{}, []).

scan(<<>>, State, Acc) ->
    Token = {end_of_input, State#state.position-1},
    lists:reverse([Token | Acc]);
% skip whitespace
scan(<<WS/utf8, Rest/binary>>, #state{position=P}=State, Acc) when ?IS_WHITESPACE(WS) -> scan(Rest, State#state{position=P+1}, Acc);
% keywords
scan(<<"drakon", Rest/binary>>, State, Acc) -> scan_keyword(drakon, 6, Rest, State, Acc);
scan(<<"primitive", Rest/binary>>, State, Acc) -> scan_keyword(primitive, 9, Rest, State, Acc);
scan(<<"silhouette", Rest/binary>>, State, Acc) -> scan_keyword(silhouette, 10, Rest, State, Acc);
scan(<<"action", Rest/binary>>, State, Acc) -> scan_keyword(action, 6, Rest, State, Acc);
scan(<<"question", Rest/binary>>, State, Acc) -> scan_keyword(question, 8, Rest, State, Acc);
scan(<<"insertion", Rest/binary>>, State, Acc) -> scan_keyword(insertion, 9, Rest, State, Acc);
scan(<<"address", Rest/binary>>, State, Acc) -> scan_keyword(address, 7, Rest, State, Acc);
scan(<<"link", Rest/binary>>, State, Acc) -> scan_keyword(link, 4, Rest, State, Acc);
scan(<<"connector", Rest/binary>>, State, Acc) -> scan_keyword(connector, 9, Rest, State, Acc);
scan(<<"end", Rest/binary>>, State, Acc) -> scan_keyword('end', 3, Rest, State, Acc);
scan(<<"yes", Rest/binary>>, State, Acc) -> scan_keyword(yes, 3, Rest, State, Acc);
scan(<<"no", Rest/binary>>, State, Acc) -> scan_keyword(no, 2, Rest, State, Acc);
% stuff
scan(<<"(*", Rest/binary>>, #state{position=P}=State, Acc) ->
    scan_stuff(Rest, State#state{position=P+2}, Acc);
% separators
scan(<<C/utf8, Rest/binary>>, State, Acc) when ?IS_SEPARATOR(C) ->
    scan_separator(C, Rest, State, Acc);
% collect all unknown as identifier
scan(Rest, State, Acc) -> scan_identifier(Rest, State, Acc).

%%
%% Helpers
%%

scan_stuff(Bin, State, Acc) ->
    {StuffData, Rest, State1} = stuff_token(Bin, State),
    Token = {stuff, StuffData, State#state.position},
    scan(Rest, State1, [Token |Acc]).

scan_identifier(Bin, State, Acc) ->
    {IdentifierData, Rest, State1} = identifier_token(Bin, State),
    Token = {identifier, IdentifierData, State#state.position},
    scan(Rest, State1, [Token |Acc]).

scan_keyword(Keyword, L, Rest, #state{position=P}=State, Acc) ->
    Token = {keyword, Keyword, P},
    scan(Rest, State#state{position=P+L}, [Token | Acc]).

scan_separator(Separator, Rest, #state{position=P}=State, Acc) ->
    Token = {Separator, P},
    scan(Rest, State#state{position=P+1}, [Token | Acc]).

stuff_token(Bin, State) -> stuff_token(Bin, State, <<>>).

stuff_token(<<>>, #state{position=P}=State, Acc) ->
    {Acc, <<>>, State#state{position=P}};
stuff_token(<<"*)", Rest/binary>>, #state{position=P}=State, Acc) ->
    {Acc, Rest, State#state{position=P+2}};
stuff_token(<<C/utf8, Rest/binary>>, #state{position=P}=State, Acc) ->
    stuff_token(Rest, State#state{position=P+1}, <<Acc/binary, C>>).

identifier_token(Bin, State) -> identifier_token(Bin, State, <<>>).

identifier_token(<<>>, #state{position=P}=State, Acc) ->
    {Acc, <<>>, State#state{position=P}};
identifier_token(<<S/utf8, _/binary>> = Bin, State, Acc) when ?IS_SEPARATOR(S) ->
    {Acc, Bin, State};
identifier_token(<<WS/utf8, _/binary>> = Bin, State, Acc) when ?IS_WHITESPACE(WS) ->
    {Acc, Bin, State};
identifier_token(<<C/utf8, Rest/binary>>, #state{position=P}=State, Acc) ->
    identifier_token(Rest, State#state{position=P+1}, <<Acc/binary, C>>).

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

scan_whitespace_test() ->
    ?assertEqual([{end_of_input, 0}], scan(<<>>)),
    ?assertEqual([{end_of_input, 5}], scan(<<"     ">>)),
    ?assertEqual([{end_of_input, 5}], scan(<<"\n\t  \r">>)),
    ok.

scan_keywords_test() ->
    ?assertEqual([
        {keyword, drakon, 1},
        {end_of_input, 6}], scan(<<"drakon">>)),
    ?assertEqual([
        {keyword, drakon, 1},
        {keyword, drakon, 8},
        {end_of_input, 13}], scan(<<"drakon drakon">>)),
    ?assertEqual([
        {keyword, drakon, 1},
        {keyword, action, 8},
        {end_of_input, 13}], scan(<<"drakon action">>)),
    ?assertEqual([
        {keyword, drakon, 1},
        {keyword, primitive, 8},
        {keyword, action, 18},
        {end_of_input, 23}], scan(<<"drakon primitive action">>)),
    ok.

scan_separators_test() ->
    ?assertEqual([{$(, 1}, {$), 2}, {end_of_input, 2}], scan(<<"()">>)),
    ?assertEqual([{${, 1}, {$}, 5}, {end_of_input, 5}], scan(<<"{   }">>)),
    ok.

scan_minimal_drakon_test() ->
        ?assertEqual([
            {keyword, drakon, 1},
            {stuff, <<" S ">>, 10},
            {keyword, primitive, 16},
            {${, 26},
            {keyword, 'end', 28},
            {$}, 32},
            {end_of_input, 32}], scan(<<"drakon (* S *) primitive { end }">>)),
        ok.

scan_minimal1_drakon_test() ->
        ?assertEqual([
            {keyword, drakon, 1},
            {$(, 8},
            {identifier, <<"S">>, 9},
            {$), 10},
            {keyword, primitive, 12},
            {${, 22},
            {keyword, 'end', 24},
            {$}, 28},
            {end_of_input, 28}], scan(<<"drakon (S) primitive { end }">>)),
        ok.


-endif.
