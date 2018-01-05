%%
%%
%%

-module(borkbork).

-export([
    parse/1,
    walk/3
]).

-include("borkbork.hrl").

parse(Filename) ->
    case file:read_file(Filename) of
        {ok, Data} ->
            Tokens = b_scanner:scan(Data),
            AST  = b_parser:parse(Tokens),
            A = AST#drakon.attributes,
            AST#drakon{attributes=maps:put(filename, Filename, A)}
    end.

walk(#drakon{}=Node, Fun, State) ->
    node(Node, Fun, State).

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
    {List1, State1} = node(List, Fun, State),
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
