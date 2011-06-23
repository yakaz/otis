%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    Var      = ?VAR,
    %% Expand variable name.
    {State1, Name} = otis_var:expand(State, Var#var.name),
    Var1 = Var#var{
      name = Name
    },
    Type_Mod = case otis_var:expected_type(Var1) of
        undefined -> ?TYPE_MOD;
        T         -> otis_utils:type_mod(T)
    end,
    %% Expand value and maybe convert it.
    {State2, Expanded} = otis_var:expand(State1, ?VALUE),
    Expected = otis_utils:test_and_convert(Type_Mod, Expanded),
    %% After expansion, it may be a non-user variable, like $(PATH).
    {State3, Value} = otis_var:get(State2, Var1, Type_Mod),
    case Value of
        Expected  when Value /= undefined -> ?NEXT_EXPR(State3);
        ""        when Expected == ""     -> ?NEXT_EXPR(State3);
        undefined when Expected == ""     -> ?NEXT_EXPR(State3);
        _                                 -> otis_utils:abort(State3)
    end.
