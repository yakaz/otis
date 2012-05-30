%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    %% Regex: ?REGEX_S
    %% Flags: ?FLAGS_S
    {State1, Regex} = otis_var:expand(State, ?REGEX),
    Flags = ?FLAGS,
    Captures = ?CAPTURES,
    %% Expand name.
    {State2, Name} = otis_var:expand(State1, ?NAME),
    %% Walk the cookies.
    State3 = otis_op_match:cookie(State2, Name, Regex, Flags,
      Captures),
    ?NEXT_EXPR(State3).
