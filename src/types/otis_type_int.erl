-module(otis_type_int).

%% Conversion.
-export([
    from_string/1,
    to_string/1
  ]).

%% -------------------------------------------------------------------
%% Conversion.
%% -------------------------------------------------------------------

from_string(String) ->
    try
        list_to_integer(String)
    catch
        _:badarg ->
            throw(conversion_failed)
    end.

to_string(Int) ->
    integer_to_list(Int).
