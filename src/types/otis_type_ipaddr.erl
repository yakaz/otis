%-
% Copyright (c) 2012-2014 Yakaz
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
% 1. Redistributions of source code must retain the above copyright
%    notice, this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright
%    notice, this list of conditions and the following disclaimer in the
%    documentation and/or other materials provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
% SUCH DAMAGE.

-module(otis_type_ipaddr).

%% Conversion.
-export([
    is_valid/1,
    compare/2,
    from_string/1,
    to_string/1
  ]).

%% -------------------------------------------------------------------
%% Comparison.
%% -------------------------------------------------------------------

is_valid({A, B, C, D}) when
  is_integer(A) andalso A >= 0 andalso A < 256 andalso
  is_integer(B) andalso B >= 0 andalso B < 256 andalso
  is_integer(C) andalso C >= 0 andalso C < 256 andalso
  is_integer(D) andalso D >= 0 andalso D < 256 ->
    true;
is_valid({A, B, C, D, E, F, G, H}) when
  is_integer(A) andalso A >= 0 andalso A < 65536 andalso
  is_integer(B) andalso B >= 0 andalso B < 65536 andalso
  is_integer(C) andalso C >= 0 andalso C < 65536 andalso
  is_integer(D) andalso D >= 0 andalso D < 65536 andalso
  is_integer(E) andalso E >= 0 andalso E < 65536 andalso
  is_integer(F) andalso F >= 0 andalso F < 65536 andalso
  is_integer(G) andalso G >= 0 andalso G < 65536 andalso
  is_integer(H) andalso H >= 0 andalso H < 65536 ->
    true;
is_valid(_) ->
    false.

compare({A, B, C, D}, {A, B, C, D}) ->
    equal;
compare({A, B, C, D, E, F, G, H}, {A, B, C, D, E, F, G, H}) ->
    equal;
compare({A, B, C, D1}, {A, B, C, D2}) when D1 < D2 ->
    less;
compare({A, B, C, D1}, {A, B, C, D2}) when D1 > D2 ->
    greater;
compare({A, B, C1, _}, {A, B, C2, _}) when C1 < C2 ->
    less;
compare({A, B, C1, _}, {A, B, C2, _}) when C1 > C2 ->
    greater;
compare({A, B1, _, _}, {A, B2, _, _}) when B1 < B2 ->
    less;
compare({A, B1, _, _}, {A, B2, _, _}) when B1 > B2 ->
    greater;
compare({A1, _, _, _}, {A2, _, _, _}) when A1 < A2 ->
    less;
compare({A1, _, _, _}, {A2, _, _, _}) when A1 > A2 ->
    greater;
compare({A, B, C, D, E, F, G, H1}, {A, B, C, D, E, F, G, H2}) when H1 < H2 ->
    less;
compare({A, B, C, D, E, F, G, H1}, {A, B, C, D, E, F, G, H2}) when H1 > H2 ->
    greater;
compare({A, B, C, D, E, F, G1, _}, {A, B, C, D, E, F, G2, _}) when G1 < G2 ->
    less;
compare({A, B, C, D, E, F, G1, _}, {A, B, C, D, E, F, G2, _}) when G1 > G2 ->
    greater;
compare({A, B, C, D, E, F1, _, _}, {A, B, C, D, E, F2, _, _}) when F1 < F2 ->
    less;
compare({A, B, C, D, E, F1, _, _}, {A, B, C, D, E, F2, _, _}) when F1 > F2 ->
    greater;
compare({A, B, C, D, E1, _, _, _}, {A, B, C, D, E2, _, _, _}) when E1 < E2 ->
    less;
compare({A, B, C, D, E1, _, _, _}, {A, B, C, D, E2, _, _, _}) when E1 > E2 ->
    greater;
compare({A, B, C, D1, _, _, _, _}, {A, B, C, D2, _, _, _, _}) when D1 < D2 ->
    less;
compare({A, B, C, D1, _, _, _, _}, {A, B, C, D2, _, _, _, _}) when D1 > D2 ->
    greater;
compare({A, B, C1, _, _, _, _, _}, {A, B, C2, _, _, _, _, _}) when C1 < C2 ->
    less;
compare({A, B, C1, _, _, _, _, _}, {A, B, C2, _, _, _, _, _}) when C1 > C2 ->
    greater;
compare({A, B1, _, _, _, _, _, _}, {A, B2, _, _, _, _, _, _}) when B1 < B2 ->
    less;
compare({A, B1, _, _, _, _, _, _}, {A, B2, _, _, _, _, _, _}) when B1 > B2 ->
    greater;
compare({A1, _, _, _, _, _, _, _}, {A2, _, _, _, _, _, _, _}) when A1 < A2 ->
    less;
compare({A1, _, _, _, _, _, _, _}, {A2, _, _, _, _, _, _, _}) when A1 > A2 ->
    greater.

%% -------------------------------------------------------------------
%% Conversion.
%% -------------------------------------------------------------------

from_string("") ->
    undefined;
from_string(String) ->
    case inet_parse:address(String) of
        {ok, IP} -> IP;
        _        -> throw(conversion_failed)
    end.

to_string(undefined) ->
    "";
to_string(IP) ->
    inet_parse:ntoa(IP).
