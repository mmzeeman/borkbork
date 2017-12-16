%%
%%
%%

-module(b_scanner).

-export([
    scan/1
]).

-record(state, {
    position = 0
}).

-define(IS_WHITESPACE(C),
    (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

-define(IS_SEPARATOR(C),
    (C =:= $( orelse C =:= $) orelse C =:= ${ orelse C =:= $} orelse C =:= $,)).

% scan(iolist()) -> borkbork:tokens()
scan(Data) ->
    scan(iolist_to_binary(Data), #state{}, []).

scan(<<>>, _State, Acc) -> lists:reverse(Acc);
% skip whitespace
scan(<<WS/utf8, Rest/binary>>, #state{position=P}=State, Acc) when ?IS_WHITESPACE(WS) -> scan(Rest, State#state{position=P+1}, Acc);
% keywords
scan(<<"drakon", Rest/binary>>, State, Acc) -> scan_keyword(drakon, 6, Rest, State, Acc);
scan(<<"primitive", Rest/binary>>, State, Acc) -> scan_keyword(primitive, 9, Rest, State, Acc);
scan(<<"silhouette", Rest/binary>>, State, Acc) -> scan_keyword(silhouette, 10, Rest, State, Acc);
scan(<<"action", Rest/binary>>, State, Acc) -> scan_keyword(action, 6, Rest, State, Acc);
scan(<<"insertion", Rest/binary>>, State, Acc) -> scan_keyword(insertion, 9, Rest, State, Acc);
% separators
scan(<<C/utf8, Rest/binary>>, State, Acc) when ?IS_SEPARATOR(C) ->
    scan_separator(C, Rest, State, Acc).

scan_keyword(Keyword, L, Rest, #state{position=Pos}=State, Acc) ->
    Token = token(keyword, Keyword, Pos),
    scan(Rest, State#state{position=Pos+L}, [Token | Acc]).

scan_separator(Separator, Rest, #state{position=Pos}=State, Acc) ->
    Token = {Separator, Pos},
    scan(Rest, State#state{position=Pos+1}, [Token | Acc]).

token(keyword, Keyword, Position) -> {keyword, Keyword, Position}.

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

scan_whitespace_test() ->
    ?assertEqual([], scan(<<>>)),
    ?assertEqual([], scan(<<"     ">>)),
    ?assertEqual([], scan(<<"\n\t  \r">>)),
    ok.

scan_keywords_test() ->
    ?assertEqual([{keyword, drakon, 0}], scan(<<"drakon">>)),
    ?assertEqual([{keyword, drakon, 0}, {keyword, drakon, 7}], scan(<<"drakon drakon">>)),
    ?assertEqual([{keyword, drakon, 0}, {keyword, action, 7}], scan(<<"drakon action">>)),
    ?assertEqual([
        {keyword, drakon, 0},
        {keyword, primitive, 7},
        {keyword, action, 17}], scan(<<"drakon primitive action">>)),
    ok.

scan_separators_test() ->
    ?assertEqual([{$(, 0}, {$), 1}], scan(<<"()">>)),
    ?assertEqual([{${, 0}, {$}, 4}], scan(<<"{   }">>)),
    ok.

-endif.
