-module(otis_type_iprange).

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

is_valid({IP1, IP2}) when
  is_tuple(IP1) andalso is_tuple(IP2) andalso
  ((size(IP1) == 4 andalso size(IP2) == 4) orelse
   (size(IP1) == 8 andalso size(IP2) == 8)) ->
    otis_type_ipaddr:is_valid(IP1) andalso otis_type_ipaddr:is_valid(IP2)
    andalso otis_type_ipaddr:compare(IP1, IP2) /= greater;
is_valid(_) ->
    false.

match({IP_Min, IP_Max}, IP)
  when is_tuple(IP) andalso size(IP_Min) == size(IP) ->
    case otis_type_ipaddr:compare(IP, IP_Min) of
        less ->
            false;
        _ ->
            case otis_type_ipaddr:compare(IP, IP_Max) of
                greater -> false;
                _       -> true
            end
    end;
match(_, _) ->
    false.

%% -------------------------------------------------------------------
%% Conversion.
%% -------------------------------------------------------------------

from_string(String) ->
    case string:tokens(String, " ") of
        [S1, S2] ->
            IP1 = otis_type_ipaddr:from_string(S1),
            IP2 = otis_type_ipaddr:from_string(S2),
            Range = {IP1, IP2},
            case is_valid(Range) of
                true  -> Range;
                false -> throw(conversion_failed)
            end;
        _ ->
            throw(conversion_failed)
    end.

to_string({IP1, IP2}) ->
    otis_type_ipaddr:to_string(IP1) ++ " " ++ otis_type_ipaddr:to_string(IP2).
