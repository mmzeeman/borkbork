%%
%%
%%

-module(b_parser).

-export([
    parse/1
]).

-record(drakon, {
    name,
    parameters,

    diagram % primitive | silhouette
}).

-record(primitive, {
    skewer % The skewer's end-point should have "end" as end_point
}).

-record(silhouette, {
    branches = []
}).

-record(branch, {
    header,

    skewer
}).

-record(skewer, {
    list = [], % The sequence of items on the skewer.

    end_point = undefined % "end" | address The label to which the skewer connects.
}).

-record(action, {
    icon_content
}).

% parse(borkbork:tokens()) -> borkbork:parse_tree()
parse([{keyword, drakon, P} | Rest]) ->
    {Stuff, Rest1} = parse_stuff(Rest),
    {Parameters, Rest2} = optional_parameters(Rest1),
    {Diagram, Rest4} = case Rest2 of
        [{keyword, primitive, P2} | Rest3] ->
            parse_primitive(Rest3);
        [{keyword, silhouette, P2} | Rest3] ->
            parse_silhouette(Rest3);
        _ -> syntax_error()
    end,

    #drakon{
        name = Stuff,
        parameters = Parameters,

        diagram = Diagram
    }.

parse_primitive([{keyword, primitive, P} | Rest]) ->
    ok.

parse_silhouette([{keyword, silhouette, P} | Rest]) ->
    ok.

syntax_error() ->
    throw(syntax_error).

%%
%% Helpers
%%

parse_stuff([{stuff, Data, Position} | Rest]) ->
    no.

optional_parameters([{} | Rest]) ->
    no.

recognize_stuff([]) ->
    ok.

recognize_parameters([]) ->
    ok.

parse_skewer(Tokens) ->
    {List, Rest} = parse_skewer(Tokens, []),
    {EndPoint, Rest2} = case Rest of
        [{keyword, 'end', _P} | Rest1]  -> {'end', Rest1};
        _ -> {undefined, Rest}
    end,
    {#skewer{list=List, end_point=EndPoint}, Rest2}.

parse_skewer([{keyword, action, _P} | _]=ActionStart, Acc) ->
    {Action, Rest} = parse_action(ActionStart),
    parse_skewer(Rest, [Action|Acc]);
parse_skewer([{keyword, insertion, _P} | _]=InsertionStart, Acc) ->
    {Insertion, Rest} = parse_insertion(InsertionStart),
    parse_skewer(Rest, [Insertion|Acc]);
parse_skewer([{keyword, question, _P} | _]=QuestionStart, Acc) ->
    {Question, Rest} = parse_question(QuestionStart),
    parse_skewer(Rest, [Question|Acc]);
parse_skewer(Unknown, Acc) ->
    {lists:reverse(Acc), Unknown}.

parse_action([{keyword, action, _P} | [H|T]]) ->
    {IconContent, Rest} = case H of
        {stuff, Stuff, _StuffP1} -> {Stuff, T};
        _ -> todo
    end,
    {#action{icon_content=IconContent}, Rest}.

parse_insertion(L) -> ok.
parse_question(L) -> ok.


%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_skewer_test() ->
    T1 = b_scanner:scan(""),
    ?assertEqual({#skewer{}, [{end_of_input, 0}]}, parse_skewer(T1)),

    T2 = b_scanner:scan("end"),
    ?assertEqual({#skewer{end_point='end'},
        [{end_of_input, 3}]}, parse_skewer(T2)),

    T3 = b_scanner:scan("action (* foo *) end"),
    ?assertEqual({#skewer{
            list=[{action,<<" foo ">>}],
            end_point='end'},
        [{end_of_input,20}]}, parse_skewer(T3)),

    ok.


-endif.