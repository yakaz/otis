%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    {State1, String} = otis_var:expand(State, ?FORMAT),
    String1 = otis_op_log:convert_http_newlines(String),
    ?ERROR(String1, []),
    ?NEXT_EXPR(State1).
