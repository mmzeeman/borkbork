%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% @author Maas-Maarten Zeeman
%% @copyright (c) 2018 Maas-Maarten Zeeman <me@mmzeeman.nl>

-module(b_drakon).


-export([
    svg/1
]).

-include("borkbork.hrl").


svg(#drakon{}) ->
     [
         element_open("svg"),
         style(),
         title_icon("Title"),

         end_icon(),
         element_close("svg")
     ].

style() ->
    [
        element_open("style"),
        text(drakon_style()),
        element_close("style")
    ].

drakon_style() ->
    "/* <![CDATA[ */
            .icon {
                stroke-width: 2;
                stroke:rgb(0,0,0);
                fill: rgb(255,255,255);
            }

            .icon-text {
                font-family: Arial, sans-serif;
                font-size: 24;
                fill: rgb(0,0,0);
                text-anchor: middle;
                alignment-baseline: middle;
            }

            .skewer {
                stroke-width: 2;
                stroke:rgb(0,0,0);
            }
            /* ]]> */
    ".

title_icon(Text) ->
    [
        element_void(rect, [
            {class, icon},
            {width, 200},
            {height, 80},
            {rx, 40},
            {ry, 40}
         ]),

        element_open(text),
        text(Text),
        element_close(text)
    ].

end_icon() ->
    [
        element_void(rect, [
            {class, icon},

            {width, 150},
            {height, 80},

            {rx, 40},
            {ry, 40}
         ]),

        element_open(text),
        text("End"),
        element_close(text)
    ].


%%
%% Helpers
%%

element_open(Tag) ->
    element_open(Tag, []).

element_open(Tag, []) ->
    B = z_convert:to_binary(Tag),
    <<$<, B/binary, $>>>;
element_open(Tag, Attributes) ->
    B = z_convert:to_binary(Tag),
    BinAttrs = iolist_to_binary([flatten_attr(A) || A <- Attributes]),
    <<$<, B/binary, 32, BinAttrs/binary, $> >>.

element_void(Tag) -> element_void(Tag, []).

element_void(Tag, []) ->
    B = z_convert:to_binary(Tag),
    <<$<, B/binary, 32, $/, $>>>;
element_void(Tag, Attributes) ->
    B = z_convert:to_binary(Tag),
    BinAttrs = iolist_to_binary([flatten_attr(A) || A <- Attributes]),
    <<$<, B/binary, 32, BinAttrs/binary, 32, $/, $> >>.

text(Text) ->
    z_html:escape_html_text(z_convert:to_binary(Text), <<>>).

element_close(Tag) ->
    B = z_convert:to_binary(Tag),
    <<$<, $/, B/binary, $>>>.

flatten_attr({N, V}) ->
    BN = z_convert:to_binary(N),
    BV = z_convert:to_binary(V),
    <<BN/binary, $=, $", BV/binary, $">>.


%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

element_open_test() ->
    ?assertEqual(<<"<tag>">>, element_open(tag)),
    ?assertEqual(<<"<tag>">>, element_open(tag, [])),
    ?assertEqual(<<"<tag foo=\"bar\">">>, element_open(tag, [{foo, bar}])),
    ok.

text_test() ->
    ?assertEqual(<<"text">>, text("text")).

element_close_test() ->
    ?assertEqual(<<"</tag>">>, element_close(tag)).

svg_test() ->
    ?DEBUG(svg(#drakon{})).

-endif.


