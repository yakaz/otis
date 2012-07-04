%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    {State1, Value} = otis_var:expand(State, ?VALUE),
    ?FUNCTION2(State1, Value).

?FUNCTION2(#state{?MEMBER = Value} = State, {IP1, IP2})
  when is_tuple(Value) andalso is_tuple(IP1) andalso is_tuple(IP2) ->
    case otis_type_iprange:match({IP1, IP2}, Value) of
        true  -> ?NEXT_EXPR(State);
        false -> otis_utils:abort(State)
    end;
?FUNCTION2(State, Value_S) when is_list(Value_S) ->
    %% Convert from string to IP address range.
    %% FIXME: How to handle conversion failure?
    Value_C = otis_type_iprange:from_string(Value_S),
    ?FUNCTION2(State, Value_C);
?FUNCTION2(State, _) ->
    otis_utils:abort(State).
