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

-record(skewer, {
    list = [], % The sequence of items on the skewer.

    attributes = #{}
}).

-record(branch, {
    icon_content,
    skewer,
    address,

    attributes = #{}
}).

-record(question, {
    icon_content,

    right_label,
    right_skewer,
    right_address,

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

-record(address, {
    icon_content,

    attributes = #{}
}).

-record(link, {
    identifier,

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

parse_primitive([{keyword, primitive, P} | Rest]) ->
    Rest1 = skip(required, ${, Rest),
    {Skewer, Rest2} = parse_skewer(Rest1),
    Rest3 = skip(required, $}, Rest2),

    %% Optional end
    Rest5 = case Rest3 of
        [{keyword, 'end', _P2} | Rest4]  -> Rest4;
        _ -> Rest3
    end,

    {#primitive{skewer=Skewer, attributes = #{start_position => P}}, Rest5}.

parse_silhouette([{keyword, silhouette, P} | Rest]) ->
    Rest1 = skip(required, ${, Rest),
    {Branches, Rest2} = parse_branches(Rest1),
    Rest3 = skip(required, $}, Rest2),

    {#silhouette{branches=Branches, attributes = #{start_position => P}}, Rest3}.

parse_branches([{keyword, branch, P}|_]=Tokens) ->
    {Branches, Rest} = parse_branches(Tokens, []);
parse_branches(Tokens) ->
    syntax_error("Expected a branch", Tokens).

parse_branches([{keyword, branch, P}|Rest]=BranchStart, Acc) ->
    {Branch, Rest1} = parse_branch(BranchStart),
    parse_branches(Rest1, [Branch|Acc]);
parse_branches(Tokens, Acc) ->
    {lists:reverse(Acc), Tokens}.

parse_branch([{keyword, branch, P}|Rest]) ->
    {IconContent, Rest1} = parse_icon_content(Rest),
    Rest2 = skip(required, ${, Rest1),
    {Skewer, Rest3} = parse_skewer(Rest2),
    Rest4 = skip(required, $}, Rest3),

    % (address|end)
    {AddressOrEnd, Rest6} = case Rest4 of
        [{keyword, 'end', EndPosition} | Rest5] -> {'end', Rest5};
        [{keyword, address, _}|_]=AddressStart -> parse_address(AddressStart);
        _ -> syntax_error("Expected an address or end", Rest4)
    end,

    {#branch{icon_content=IconContent, skewer=Skewer, address=AddressOrEnd, attributes=#{start_position => P}}, Rest6};

parse_branch(Tokens) ->
    syntax_error("Expected a branch", Tokens).




syntax_error() -> syntax_error(unkown).
syntax_error(Message) -> syntax_error(Message, []).
syntax_error(Message, Tokens) -> throw({syntax_error, Message, Tokens}).

%%
%% Helpers
%%

parse_skewer([Token|_]=Tokens) ->
    {List, Rest} = parse_skewer(Tokens, []),
    {#skewer{list=List, attributes=#{start_position => position(Token)} }, Rest}.

position({keyword, Keyword, Position}) when is_atom(Keyword) -> Position;
position({Char, Position}) when is_integer(Char) -> Position;
position({end_of_input, Position}) -> Position.


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
parse_icon_content([{$(, _} | Rest]) ->
    {Identifier, Rest1} = parse_identifier(Rest),
    Rest2 = skip(required, $), Rest1),
    {Identifier, Rest2};
parse_icon_content(Rest) -> syntax_error("Expected icon content", Rest).

parse_identifier([{identifier, Identifier, _}| Rest]) -> {Identifier, Rest};
parse_identifier(Rest) -> syntax_error("Expected an identifier", Rest).

parse_question([{keyword, question, P}|Rest]) ->
    {IconContent, Rest1} = parse_icon_content(Rest),

    %% yes | no
    {RightLabel, Rest3} = case Rest1 of
        [{keyword, yes, _P1}|Rest2] -> {yes, Rest2};
        [{keyword, no, _P1}|Rest2] -> {no, Rest2};
        _ -> syntax_error()
    end,

    %% {
    Rest5 = skip(required, ${, Rest3),

    %% Skewer
    {Skewer, Rest6} = parse_skewer(Rest5),

    %% Required }
    Rest7 = skip(required, $}, Rest6),

    %% Optional link | address
    {LinkOrAddress, Rest8} = case Rest7 of
        [{keyword, address, _PAddress}|_]-> parse_address(Rest7);
        [{keyword, link, _PLink}|_] -> parse_link(Rest7);
        _ -> {undefined, Rest7}
    end,

    %% Done

    {#question{
        icon_content = IconContent,

        right_label = RightLabel,
        right_skewer = Skewer,
        right_address = LinkOrAddress,

        attributes=#{start_position => P}
    }, Rest8}.

parse_address([{keyword, address, P}|Rest]) ->
    {IconContent, Rest1} = parse_icon_content(Rest),
    {#address{icon_content=IconContent, attributes=#{start_position => P}}, Rest1}.

parse_link([{keyword, link, P}|Rest]) ->
    Rest1 = skip(required, $(, Rest),
    {Identifier, Rest2} = parse_identifier(Rest1),
    Rest3 = skip(required, $), Rest2),
    {#link{identifier=Identifier, attributes=#{start_position => P}}, Rest3}.

%%
%% Helpers
%%

skip(_, Char, [{Char, _P}|Rest]) -> Rest;
skip(required, Char, Rest) -> syntax_error("Missing " ++ [Char], Rest);
skip(optional, _Char, [_|Rest]) -> Rest.


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
    T1 = b_scanner:scan("drakon (* test *) primitive { } end"),
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

parse_primitive_with_question_test() ->
    T1 = b_scanner:scan("drakon (test) primitive { question(is_test) no { }  }"),
    ?assertMatch(#drakon{
        name = <<"test">>,
        parameters = undefined,
        diagram = #primitive{
            skewer = #skewer{
                list = [
                    #question{
                        icon_content = <<"is_test">>,
                        right_label = no,
                        right_skewer = #skewer{
                            list = []
                        },
                        right_address = undefined,
                        attributes = _
                    }
                ],
                attributes = _
            }
        }
    },  parse(T1)),
    ok.

parse_silhouette_test() ->
    T1 = b_scanner:scan("drakon (test) silhouette{ branch(a) { } address(b) branch(b) {} end  }"),
    ?assertMatch(#drakon{
        name = <<"test">>,
        parameters = undefined,
        diagram = #silhouette{
            branches = [
                #branch{icon_content = <<"a">>, skewer = _, address = _,  attributes = _},
                #branch{icon_content = <<"b">>, skewer = _, address = 'end', attributes = _}
            ],
            attributes = _
        }
    }, parse(T1)).


-endif.