%% Source: ?SRC_GFILE:?SRC_GLINE,?SRC_GCOL (global)
%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL (rule-specific)

?FUNCTION(State) ->
    State1 = try
        ?HOOK_EXPR(State)
    catch
        throw:{abort, _} -> State
    end,
    ?FIRST_EXPR(State1).
