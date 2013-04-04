%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    {State1, Regex} = otis_var:expand(State, ?REGEX),
    Flags = ?FLAGS,
    {State2, Value} = otis_var:expand(State1, ?VALUE),
    %% Expand name.
    {State3, Name} = otis_var:expand(State2, ?NAME),
    %% Walk the headers.
    State4 = otis_op_subst:rheader(State3, Name, Regex, Flags, Value),
    ?NEXT_EXPR(State4).
