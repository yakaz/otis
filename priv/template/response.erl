%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    %% Set a default reason if none was provided by the user.
    State1 = State#state{
      response = true,
      code     = ?CODE,
      reason   = ?REASON
    },
    otis_utils:stop(State1).
