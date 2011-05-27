%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    {State1, Value} = otis_var:expand(State, ?VALUE),
    ?FUNCTION2(State1, Value).

?FUNCTION2(#state{?MEMBER = Value} = State, Value) ->
    ?NEXT_EXPR(State);
?FUNCTION2(State, _) ->
    otis_utils:abort(State).
