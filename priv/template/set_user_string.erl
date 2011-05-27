%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    Var = ?VAR,
    {State1, Name}  = otis_var:expand(State,  Var#var.name),
    {State2, Value} = otis_var:expand(State1, ?VALUE),
    %% After expansion, it may be a non-user variable, like $(PATH).
    Var1 = Var#var{
      name = Name
    },
    State3 = otis_var:set(State2, Var1, Value),
    ?NEXT_EXPR(State3).
