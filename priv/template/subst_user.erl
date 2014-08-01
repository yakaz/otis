%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    Var = ?VAR,
    {State1, Name}  = otis_var:expand(State,  Var#var.name),
    {State2, Regex} = otis_var:expand(State1, ?REGEX),
    {State3, Repl}  = otis_var:expand(State2, ?VALUE),
    %% After expansion, it may be a non-user variable, like $(PATH).
    Var1 = Var#var{
      name = Name
    },
    {State4, Value} = otis_var:get(State3, Var1),
    Flags  = ?FLAGS,
    Value1 = re:replace(Value, Regex, Repl, Flags),
    State5 = otis_var:set(State4, Var1, Value1),
    ?NEXT_EXPR(State5).
