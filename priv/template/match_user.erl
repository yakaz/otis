%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    %% Regex: ?REGEX_S
    %% Flags: ?FLAGS_S
    Var = ?VAR,
    {State1, Name}  = otis_var:expand(State,  Var#var.name),
    {State2, Regex} = otis_var:expand(State1, ?REGEX),
    %% After expansion, it may be a non-user variable, like $(PATH).
    Var1 = Var#var{
      name = Name
    },
    {State3, Value} = otis_var:get(State2, Var1),
    case Value of
        undefined ->
            otis_utils:abort(State3);
        _ ->
            Flags    = ?FLAGS,
            Captures = ?CAPTURES,
            case re:run(Value, Regex, Flags) of
                {match, Captured} ->
                    State4 = otis_op_match:handle_captures(State3,
                      Captures, Captured),
                    ?NEXT_EXPR(State4);
                nomatch ->
                    otis_utils:abort(State3)
            end
    end.
