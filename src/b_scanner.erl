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

% scan(iolist()) -> borkbork:tokens()
scan(Data) ->
    scan(iolist_to_binary(Data), #state{}, []).

scan(<<>>, _State, Acc) -> lists:reverse(Acc);
scan(<<WS/utf8, Rest/binary>>, #state{position=P}=State, Acc) when ?IS_WHITESPACE(WS) -> scan(Rest, State#state{position=P+1}, Acc);
scan(<<"drakon", Rest/binary>>, #state{position=Pos}=State, Acc) ->
    Token = token(keyword, drakon, Pos),
    scan(Rest, State#state{position=Pos+6}, [Token | Acc]).

token(keyword, Keyword, Position) -> {keyword, Keyword, Position}.

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

scan_whitespace_test() ->
    ?assertEqual([], scan(<<>>)),
    ?assertEqual([], scan(<<"     ">>)),
    ok.

scan_keywords_test() ->
    ?assertEqual([{keyword, drakon, 0}], scan(<<"drakon">>)),
    ?assertEqual([{keyword, drakon, 0}, {keyword, drakon, 7}], scan(<<"drakon drakon">>)),
    ok.

-endif.
