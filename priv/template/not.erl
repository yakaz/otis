%%# Operator "not".
%%#
%%# Call "child" operators. If one fails, return the state returned.
%%# Otherse, abort.
%%#
%%# Template variables:
%%#   ?FUNCTION: function name
%%#   ?SUB_EXPR: child operator

%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    Ret = try
        ?SUB_EXPR(State)
    catch
        throw:{abort, State1} ->
            {true, State1}
    end,
    case Ret of
        {true, State2}  -> ?NEXT_EXPR(State2);
        {false, State2} -> otis_utils:abort(State2)
    end.

?FUNCTION2(State) ->
    {false, State}.
