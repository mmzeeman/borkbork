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

    diagram, % primitive | silhouette

    attributes = #{}
}).

-record(primitive, {
    skewer,

    attributes = #{}
}).

-record(silhouette, {
    branches = [],

    attributes = #{}
}).

-record(branch, {
    header,

    skewer,

    attributes = #{}
}).

-record(skewer, {
    list = [], % The sequence of items on the skewer.

    attributes = #{}
}).

-record(action, {
    icon_content,

    attributes = #{}
}).

-record(insertion, {
    icon_content,

    attributes = #{}
}).

% parse(borkbork:tokens()) -> borkbork:parse_tree()
parse([{keyword, drakon, P} | Rest]) ->
    {IconContent, Rest1} = parse_icon_content(Rest),
    {Parameters, Rest2} = optional_parse_icon_content(Rest1),
    {Diagram, Rest4} = case Rest2 of
        [{keyword, primitive, _} | _] = PrimitiveStart ->
            parse_primitive(PrimitiveStart);
        [{keyword, silhouette, _} | _] = SilhouetteStart ->
            parse_silhouette(SilhouetteStart);
        _ -> syntax_error()
    end,

    done = case Rest4 of
        [] -> done;
        [{end_of_input, _}] -> done;
        _ -> syntax_error()
    end,

    #drakon{
        name = IconContent,
        parameters = Parameters,

        diagram = Diagram,

        attributes = #{start_position => P}
    }.

parse_primitive([{keyword, primitive, P}, {${, _P1} | Rest]) ->
    {Skewer, Rest1} = parse_skewer(Rest),

    Rest3 = case Rest1 of
        [{keyword, 'end', _P2} | Rest2]  -> Rest2;
        _ -> Rest1
    end,

    Rest5 = case Rest3 of
        [{$}, _P3} | Rest4] -> Rest4;
        _ -> syntax_error() % missing }
    end,

    {#primitive{skewer=Skewer, attributes = #{start_position => P}}, Rest5}.

parse_silhouette([{keyword, silhouette, _P} | _Rest]) ->
    ok.

syntax_error() ->
    throw(syntax_error).

%%
%% Helpers
%%

parse_skewer(Tokens) ->
    {List, Rest} = parse_skewer(Tokens, []),
    {#skewer{list=List}, Rest}.

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

parse_action([{keyword, action, P} | Rest]) ->
    {IconContent, Rest1} = parse_icon_content(Rest),
    {#action{icon_content=IconContent, attributes=#{start_position=>P}}, Rest1}.

parse_insertion([{keyword, insertion, P} | Rest]) ->
    {IconContent, Rest1} = parse_icon_content(Rest),
    {#insertion{icon_content=IconContent, attributes=#{start_position=>P}}, Rest1}.

optional_parse_icon_content([{stuff, _, _}|_]=IconStart) -> parse_icon_content(IconStart);
optional_parse_icon_content([{$(, _}|_]=IconStart) -> parse_icon_content(IconStart);
optional_parse_icon_content(Rest) -> {undefined, Rest}.

parse_icon_content([{stuff, Stuff, _P} | Rest]) -> {Stuff, Rest};
parse_icon_content([{$(, _}, {identifier, Identifier, _}, {$), _} | Rest]) ->
    {Identifier, Rest};
parse_icon_content(_) ->
    syntax_error().

parse_question(_L) -> todo.

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_skewer_test() ->
    T1 = b_scanner:scan(""),
    ?assertMatch({#skewer{
        list=[],
        attributes=_}, [{end_of_input, 0}]}, parse_skewer(T1)),

    T2 = b_scanner:scan("end"),
    ?assertMatch({#skewer{
        list=[],
        attributes=_}, [{keyword, 'end', 1}, {end_of_input, 3}]}, parse_skewer(T2)),

    T3 = b_scanner:scan("action (* foo *) end"),
    ?assertMatch({#skewer{
            list=[
                #action{icon_content= <<" foo ">>, attributes=_}
            ],
            attributes=_},
        [{keyword, 'end', 18}, {end_of_input, 20}]}, parse_skewer(T3)),

    ok.

parse_primitive_drakon_test() ->
    T1 = b_scanner:scan("drakon (* test *) primitive { end }"),
    ?assertMatch(#drakon{
        name = <<" test ">>,
        parameters = undefined,
        diagram = #primitive{
            skewer = #skewer{
                list=[],
                attributes=_
            }
        },
        attributes=_
    }, parse(T1)),
    ok.

-endif.