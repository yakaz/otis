%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(#state{?MEMBER = Value} = State) ->
    {State1, Regex} = otis_var:expand(State,  ?REGEX),
    {State2, Repl}  = otis_var:expand(State1, ?VALUE),
    Flags  = ?FLAGS,
    Value1 = re:replace(Value, Regex, Repl, Flags),
    State3 = State2#state{
      ?MEMBER = Value1
    },
    ?NEXT_EXPR(State3).
