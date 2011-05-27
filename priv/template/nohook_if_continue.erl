?FUNCTION(_, New_State, first)    -> ?FIRST_RULE(New_State);
?FUNCTION(_, New_State, previous) -> ?PREV_RULE(New_State);
?FUNCTION(_, New_State, next)     -> ?NEXT_RULE(New_State);
?FUNCTION(_, New_State, last)     -> ?LAST_RULE(New_State);
?FUNCTION(_, New_State, Name)     -> call_rule(New_State, Name).
