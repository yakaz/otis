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

-module(otis_type_ipmask).

%% Conversion.
-export([
    is_valid/1,
    match/2,
    from_string/1,
    to_string/1
  ]).

%% -------------------------------------------------------------------
%% Comparison.
%% -------------------------------------------------------------------

is_valid({IP, Bits}) when
  is_tuple(IP) andalso is_integer(Bits) andalso
  ((size(IP) == 4 andalso Bits > 0 andalso Bits =< 32) orelse
   (size(IP) == 8 andalso Bits > 0 andalso Bits =< 128)) ->
    otis_type_ipaddr:is_valid(IP);
is_valid(_) ->
    false.

match({{A1, B1, C1, D1}, Bits}, {A2, B2, C2, D2}) ->
    match2(Bits, 8,
      [A1, B1, C1, D1],
      [A2, B2, C2, D2]);
match({{A1, B1, C1, D1, E1, F1, G1, H1}, Bits},
  {A2, B2, C2, D2, E2, F2, G2, H2}) ->
    match2(Bits, 16,
      [A1, B1, C1, D1, E1, F1, G1, H1],
      [A2, B2, C2, D2, E2, F2, G2, H2]);
match(_, _) ->
    false.

match2(0, _, _, _) ->
    true;
match2(Bits, Bits_Per_Block, [B | Blocks1], [B | Blocks2])
  when Bits >= Bits_Per_Block ->
    match2(Bits - Bits_Per_Block, Bits_Per_Block, Blocks1, Blocks2);
match2(Bits, Bits_Per_Block, [B1 | _], [B2 | _])
  when Bits < Bits_Per_Block ->
    Shift = Bits_Per_Block - Bits,
    (B1 bsr Shift) == (B2 bsr Shift);
match2(_, _, _, _) ->
    false.

%% -------------------------------------------------------------------
%% Conversion.
%% -------------------------------------------------------------------

from_string(String) ->
    case string:tokens(String, "/") of
        [S1, S2] ->
            IP   = otis_type_ipaddr:from_string(S1),
            Bits = try
                list_to_integer(S2)
            catch
                _:badarg ->
                    throw(conversion_failed)
            end,
            Mask = {IP, Bits},
            case is_valid(Mask) of
                true  -> Mask;
                false -> throw(conversion_failed)
            end;
        _ ->
            throw(conversion_failed)
    end.

to_string({IP, Bits}) ->
    otis_type_ipaddr:to_string(IP) ++ "/" ++ integer_to_list(Bits).
