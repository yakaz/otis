-module(otis_type_port).

%% Conversion.
-export([
    is_valid/1,
    from_string/1,
    to_string/1
  ]).

-define(IS_PORT(P), (P >= 0 andalso P =< 65535)).

%% -------------------------------------------------------------------
%% Comparison.
%% -------------------------------------------------------------------

is_valid(Port) when is_integer(Port) andalso ?IS_PORT(Port) ->
    true;
is_valid(_) ->
    false.

%% -------------------------------------------------------------------
%% Conversion.
%% -------------------------------------------------------------------

from_string(String) ->
    Int = otis_type_int:from_string(String),
    if
        ?IS_PORT(Int) -> Int;
        true          -> throw(conversion_failed)
    end.

to_string(Int) ->
    otis_type_int:to_string(Int).
