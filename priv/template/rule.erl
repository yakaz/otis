%% -------------------------------------------------------------------
%% ?NAME
%%
?DESC
%%
%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL
%% -------------------------------------------------------------------

%%# Rule entry point.
%%#
%%# If the rule fails, the next rule is called with the original state.
%%# If the rule succeeds, the processing stops here and the new state is
%%# returned. If the rule contains a "goto" or a "stop" expression, the
%%# generated code will call the targeted rule function name directly.
%%#
%%# Template variables:
%%#   ?FUNCTION:   function name
%%#   ?FIRST_EXPR: function name of the first expression

?FUNCTION(State) ->
    State1 = State#state{
      rule_name = ?NAME_S
    },
    try
        State3 = ?RULE_INIT_HOOK(State1),
        ?IF_FINAL_HOOK(State1, State3)
    catch
        throw:{abort, State2}   -> ?IF_ABORT_HOOK(State1, State2);
        throw:{stop, State2}    -> ?IF_FINAL_HOOK(State1, State2);
        throw:{Next, State2}    -> ?IF_CONTINUE_HOOK(State1, State2, Next);
        throw:conversion_failed -> ?IF_ABORT_HOOK(State1, State1)
    end.
