%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    {State1, Value} = otis_var:expand(State, ?VALUE),
    ?FUNCTION2(State1, Value).

?FUNCTION2(State, Value) when is_integer(Value) ->
    case otis_type_port:is_valid(Value) of
        true ->
            State1 = State#state{
              ?MEMBER = Value
            },
            ?NEXT_EXPR(State1);
        false ->
            throw(conversion_failed)
    end;
?FUNCTION2(State, Value_S) when is_list(Value_S) ->
    %% Convert from string to TCP port.
    %% FIXME: How to handle conversion failure?
    Value_C = otis_type_port:from_string(Value_S),
    State1 = State#state{
      ?MEMBER = Value_C
    },
    ?NEXT_EXPR(State1);
?FUNCTION2(_, _) ->
    throw(conversion_failed).
