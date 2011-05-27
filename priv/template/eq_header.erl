%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    %% Expand name and value.
    {State1, Name}  = otis_var:expand(State, ?NAME),
    {State2, Value} = otis_var:expand(State1, ?VALUE),
    %% Walk the headers.
    State3 = otis_op_eq:header(State2, Name, Value,
      ?TYPE_MOD),
    ?NEXT_EXPR(State3).
