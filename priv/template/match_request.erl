%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(#state{?MEMBER = Value} = State) ->
    %% Regex: ?REGEX_S
    %% Flags: ?FLAGS_S
    {State1, Regex} = otis_var:expand(State, ?REGEX),
    Flags    = ?FLAGS,
    Captures = ?CAPTURES,
    case re:run(Value, Regex, Flags) of
        {match, Captured} ->
            State2 = otis_op_match:handle_captures(State1,
              Captures, Captured),
            ?NEXT_EXPR(State2);
        nomatch ->
            otis_utils:abort(State1)
    end.
