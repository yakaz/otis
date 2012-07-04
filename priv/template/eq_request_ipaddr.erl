%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    {State1, Value} = otis_var:expand(State, ?VALUE),
    ?FUNCTION2(State1, Value).

?FUNCTION2(#state{?MEMBER = Value} = State, Value) when is_tuple(Value) ->
    ?NEXT_EXPR(State);
?FUNCTION2(State, Value_S) when is_list(Value_S) ->
    %% Convert from string to IP address.
    %% FIXME: How to handle conversion failure?
    Value_C = otis_type_ipaddr:from_string(Value_S),
    ?FUNCTION2(State, Value_C);
?FUNCTION2(State, _) ->
    otis_utils:abort(State).
