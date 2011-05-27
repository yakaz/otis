%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    {State1, String} = otis_var:expand(State, ?FORMAT),
    ?ERROR(String, []),
    ?NEXT_EXPR(State1).
