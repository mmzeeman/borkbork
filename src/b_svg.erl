%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% @author Maas-Maarten Zeeman
%% @copyright (c) 2018 Maas-Maarten Zeeman <me@mmzeeman.nl>

-module(b_svg).

-export([estimate_text_width/2]).

% @doc Make a crude estimate what the width of the text will be.
estimate_text_width(Text, FontSize) -> round(FontSize * (estimate_width(Text, 0) / 15.0)).

%%
%% Helpers
%%

estimate_width(<<>>, Width) -> Width;
estimate_width(<<C/utf8, Rest/binary>>, Width) ->
    W = char_width(C),
    estimate_width(Rest, Width + W).

char_width($W) -> 15;
char_width($M) -> 15;
char_width($w) -> 12;
char_width($m) -> 12;
char_width($I) -> 4;
char_width($i) -> 4;
char_width($l) -> 4;
char_width($t) -> 4;
char_width($f) -> 4;
char_width($r) -> 8;
char_width(Char) ->
    case Char =:= string:to_upper(Char) of
        true -> 12;
        false -> 10
    end.

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

estimate_width_test() ->
    ?assertEqual(72, estimate_text_width(<<"Maas">>, 24)),
    ?assertEqual(261, estimate_text_width(<<"Willem van Oranje">>, 24)),
    ok.


-endif.