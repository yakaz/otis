-module(otis_type_port).

%% Conversion.
-export([
    from_string/1,
    to_string/1
  ]).

%% -------------------------------------------------------------------
%% Conversion.
%% -------------------------------------------------------------------

from_string(String) ->
    Int = otis_type_int:from_string(String),
    if
        Int >= 0 andalso Int =< 65535 ->
            Int;
        true ->
            throw(conversion_failed)
    end.

to_string(Int) ->
    otis_type_int:to_string(Int).
