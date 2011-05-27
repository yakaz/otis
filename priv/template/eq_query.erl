%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    %% Expand name and value.
    {State1, Name}  = otis_var:expand(State, ?NAME),
    {State2, Value} = otis_var:expand(State1, ?VALUE),
    %% Walk the query parameters.
    State3 = otis_op_eq:query_param(State2, Name, Value,
      ?TYPE_MOD),
    ?NEXT_EXPR(State3).
