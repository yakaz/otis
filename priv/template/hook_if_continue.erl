%% Source: ?SRC_GFILE:?SRC_GLINE,?SRC_GCOL (global)
%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL (rule-specific)

?FUNCTION(Orig_State, New_State, Next) ->
    try
        State1 = ?HOOK_EXPR(New_State#state{other_state = Orig_State}),
        State3 = State1#state{other_state = undefined},
        case Next of
            first    -> ?FIRST_RULE(State3);
            previous -> ?PREV_RULE(State3);
            next     -> ?NEXT_RULE(State3);
            last     -> ?LAST_RULE(State3);
            Name1    -> call_rule(State3, Name1)
        end
    catch
        throw:{abort, _}         -> ?NEXT_RULE(New_State);
        throw:{stop, State2}     -> State2;
        throw:{first, State2}    -> ?FIRST_RULE(State2);
        throw:{previous, State2} -> ?PREV_RULE(State2);
        throw:{next, State2}     -> ?NEXT_RULE(State2);
        throw:{last, State2}     -> ?LAST_RULE(State2);
        throw:{Name, State2}     -> call_rule(State2, Name)
    end.
