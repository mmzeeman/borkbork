%%
%%
%%

-module(b_walk).

-export([walk/3, get_attribute/2, put_attribute/3]).

-include("borkbork.hrl").

walk(#drakon{}=Node, Fun, State) ->
    node(Node, Fun, State).

node(#drakon{name=Name, parameters=Parameters, diagram=Diagram}=Node, Fun, State) ->
    {Name1, State1} = node({diagram_name, Name}, Fun, State),
    {Parameters1, State2} = node({diagram_parameters, Parameters}, Fun, State1),
    {Diagram1, State3} = node(Diagram, Fun, State2),

    Fun(Node#drakon{name=Name1, parameters=Parameters1, diagram=Diagram1}, State3);

node(#primitive{skewer=Skewer}=Node, Fun, State) ->
    io:fwrite("~p, ~p~n", [?LINE, Skewer]),
    {Skewer1, State1} = node(Skewer, Fun, State),
    Fun(Node#primitive{skewer=Skewer1}, State1);

node(#silhouette{branches=Branches}=Node, Fun, State) ->
    {Branches1, State1} = nodes(Branches, Fun, State),
    Fun(Node#silhouette{branches=Branches1}, State1);

node(#branch{icon_content=I, skewer=Skewer, address=A}=Node, Fun, State) ->
    {I1, State1} = node({branch, I}, Fun, State),
    {Skewer1, State2} = node(Skewer, Fun, State1),
    {A1, State3} = node({address, A}, Fun, State2),
    Fun(Node#branch{icon_content=I1, skewer=Skewer1, address=A1}, State3);

node(#skewer{list=List}=Node, Fun, State) ->
    io:fwrite("~p, ~p~n", [?LINE, List]),
    {List1, State1} = nodes(List, Fun, State),
    io:fwrite("~p, ~p~n", [?LINE, List1]),
    Fun(Node#skewer{list=List1}, State1);

node(#question{icon_content=I, right_label=L, right_skewer=Skewer, right_address=A}=Node, Fun, State) ->
    {I1, State1} = node({question, I}, Fun, State),
    {L1, State2} = node({question_right_label, L}, Fun, State1),
    {Skewer1, State3} = node(Skewer, Fun, State2),
    {A1, State4} = node({question_address, A}, Fun, State3),
    Fun(Node#question{icon_content=I1, right_label=L1, right_skewer=Skewer1, right_address=A1}, State4);

node({diagram_name, _}=Node, Fun, State) -> Fun(Node,State);
node({diagram_parameters, _}=Node, Fun, State) -> Fun(Node,State);

node({branch, _}=Node, Fun, State) -> Fun(Node, State);
node({address, _}=Node, Fun, State) -> Fun(Node, State);

node({question,_}=Node, Fun, State) -> Fun(Node, State);
node({question_right_label,_}=Node, Fun, State) -> Fun(Node, State);
node({question_address,_}=Node, Fun, State) -> Fun(Node, State);

node(#action{}=Action, Fun, State) -> Fun(Action, State);
node(#insertion{}=Insertion, Fun, State) -> Fun(Insertion, State);
node(#connector{}=Connector, Fun, State) -> Fun(Connector, State).

nodes(List, Fun, State) -> nodes(List, Fun, State, []).

nodes([], _Fun, State, Acc) -> {lists:reverse(Acc), State};
nodes([Node|Rest], Fun, State, Acc) ->
    {Node1, State1} = Fun(Node, State),
    nodes(Rest, Fun, State1, [Node1|Acc]).

get_attribute(Key, Node) ->
    maps:get(Key, attributes(Node)).

put_attribute(Key, Value, Node) ->
    Attributes1 = maps:put(Key, Value, attributes(Node)),
    set_attributes(Attributes1, Node).

attributes(#drakon{attributes=Attributes}) -> Attributes;
attributes(#primitive{attributes=Attributes}) -> Attributes;
attributes(#skewer{attributes=Attributes}) -> Attributes;
attributes(#action{attributes=Attributes}) -> Attributes.

set_attributes(A, #drakon{}=Drakon) -> Drakon#drakon{attributes=A};
set_attributes(A, #primitive{}=Primitive) -> Primitive#primitive{attributes=A};
set_attributes(A, #skewer{}=Skewer) -> Skewer#skewer{attributes=A};
set_attributes(A, #action{}=Action) -> Action#action{attributes=A}. 

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

walk_test() ->
    F = fun
        ({diagram_name, Name}, State) ->
            {Name, maps:put(name, Name, State)};
        ({diagram_parameters, Parameters}, State) ->
            {Parameters, maps:put(parameters, Parameters, State)};
        (#drakon{}=Node, State) ->
            Node1 = put_attribute(visited, true, Node),
            {Node1, State};
        (#primitive{}=Node, State) ->
            Node1 = put_attribute(visited, true, Node),
            {Node1, maps:put(kind, primitive, State)};
        (#skewer{}=Node, State) ->
            {Node, State};
        (#action{}=Node, State) ->
            Node1 = put_attribute(visited, true, Node),
            {Node1, State};
        (List, State) when is_list(List) ->
            {List, State};
        (N, State) ->
            ?DEBUG({todo, N}),
            {N, State}
    end,

    T1 = b_scanner:scan("drakon (test) primitive { action(do_it) } end"),
    Ast = b_parser:parse(T1),

    {_Ast1, State} = walk(Ast, F, #{}),

    ?assertEqual(<<"test">>, maps:get(name, State)),
    ?assertEqual(undefined, maps:get(parameters, State)),
    ?assertEqual(primitive, maps:get(kind, State)),

    ok.

-endif.