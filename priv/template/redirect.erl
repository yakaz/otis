%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    %% In case of a redirection, assure that a Location header is set.
    State1 = otis_op_response:set_location(State),
    %% Set a default reason if none was provided by the user.
    State2 = State1#state{
      response = true,
      code     = ?CODE,
      reason   = ?REASON
    },
    otis_utils:stop(State2).
