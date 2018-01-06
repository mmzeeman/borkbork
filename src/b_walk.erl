%%
%%
%%

-module(b_walk).

-export([
    walk/3,
    get_attribute/2,
    put_attribute/3
]).

-include("borkbork.hrl").

-spec walk(
    borkbork:ast_node(),
    fun((borkbork:ast_node(), any()) -> {borkbork:ast_node(), any()}),
    any()) -> {borkbork:ast_node(), any()}.
walk(#drakon{}=Node, Fun, State) -> node(Node, Fun, State).

-spec node(
    borkbork:ast_node(),
    fun((borkbork:ast_node(), any()) -> {borkbork:ast_node(), any()}),
    any()) -> {borkbork:ast_node(), any()}.
node(#drakon{name=Name, parameters=Parameters, diagram=Diagram}=Node, Fun, State) ->
    {Name1, State1} = node({diagram_name, Name}, Fun, State),
    {Parameters1, State2} = node({diagram_parameters, Parameters}, Fun, State1),
    {Diagram1, State3} = node(Diagram, Fun, State2),

    Fun(Node#drakon{name=Name1, parameters=Parameters1, diagram=Diagram1}, State3);

node(#primitive{skewer=Skewer}=Node, Fun, State) ->
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
    {List1, State1} = nodes(List, Fun, State),
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

-spec get_attribute(any(), borkbork:ast_node()) -> any().
get_attribute(Key, Node) ->
    maps:get(Key, attributes(Node)).

-spec put_attribute(any(), any(), borkbork:ast_node()) -> borkbork:ast_node().
put_attribute(Key, Value, Node) ->
    Attributes1 = maps:put(Key, Value, attributes(Node)),
    set_attributes(Attributes1, Node).

-spec attributes(borkbork:ast_node()) -> maps:map().
attributes(#drakon{attributes=Attributes}) -> Attributes;
attributes(#primitive{attributes=Attributes}) -> Attributes;
attributes(#silhouette{attributes=Attributes}) -> Attributes;
attributes(#skewer{attributes=Attributes}) -> Attributes;
attributes(#branch{attributes=Attributes}) -> Attributes;
attributes(#address{attributes=Attributes}) -> Attributes;
attributes(#link{attributes=Attributes}) -> Attributes;
attributes(#connector{attributes=Attributes}) -> Attributes;
attributes(#question{attributes=Attributes}) -> Attributes;
attributes(#action{attributes=Attributes}) -> Attributes;
attributes(#insertion{attributes=Attributes}) -> Attributes.

-spec set_attributes(maps:map(), borkbork:ast_node()) -> borkbork:ast_node().
set_attributes(A, #drakon{}=N) -> N#drakon{attributes=A};
set_attributes(A, #primitive{}=N) -> N#primitive{attributes=A};
set_attributes(A, #silhouette{}=N) -> N#silhouette{attributes=A};
set_attributes(A, #skewer{}=N) -> N#skewer{attributes=A};
set_attributes(A, #branch{}=N) -> N#branch{attributes=A};
set_attributes(A, #address{}=N) -> N#address{attributes=A};
set_attributes(A, #link{}=N) -> N#link{attributes=A};
set_attributes(A, #connector{}=N) -> N#connector{attributes=A};
set_attributes(A, #question{}=N) -> N#question{attributes=A};
set_attributes(A, #action{}=N) -> N#action{attributes=A};
set_attributes(A, #insertion{}=N) -> N#insertion{attributes=A}.

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

walk_fun({diagram_name, Name}, State) ->
    {Name, maps:put(name, Name, State)};
walk_fun({diagram_parameters, Parameters}, State) ->
    {Parameters, maps:put(parameters, Parameters, State)};
walk_fun(#drakon{}=Node, State) ->
    Node1 = put_attribute(visited, true, Node),
    {Node1, State};
walk_fun(#primitive{}=Node, State) ->
    Node1 = put_attribute(visited, true, Node),
    {Node1, maps:put(kind, primitive, State)};
walk_fun(#skewer{}=Node, State) ->
    {Node, State};
walk_fun(#action{}=Node, State) ->
    Node1 = put_attribute(visited, true, Node),
    {Node1, State};
walk_fun(List, State) when is_list(List) ->
    {List, State};
walk_fun(N, State) ->
    ?DEBUG({todo, N}),
    {N, State}.

walk_test() ->
    T1 = b_scanner:scan("drakon (test) primitive { action(do_it) } end"),
    Ast = b_parser:parse(T1),

    {_Ast1, State} = walk(Ast, fun walk_fun/2, #{}),

    ?assertEqual(<<"test">>, maps:get(name, State)),
    ?assertEqual(undefined, maps:get(parameters, State)),
    ?assertEqual(primitive, maps:get(kind, State)),

    ok.

walk2_test() ->
    T1 = b_scanner:scan("drakon (test) (test1) primitive { action(do_it) } end"),
    Ast = b_parser:parse(T1),

    {_Ast1, State} = walk(Ast, fun walk_fun/2, #{}),

    ?assertEqual(<<"test">>, maps:get(name, State)),
    ?assertEqual(<<"test1">>, maps:get(parameters, State)),
    ?assertEqual(primitive, maps:get(kind, State)),

    ok.

-endif.