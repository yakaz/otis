%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    {State1, Value} = otis_var:expand(State, ?VALUE),
    State2 = State1#state{
      ?MEMBER = Value
    },
    ?NEXT_EXPR(State2).
