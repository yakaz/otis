%%# Operator "all".
%%#
%%# Call "child" operators until one fails and abort in this case.
%%#
%%# Template variables:
%%#   ?FUNCTION: function name
%%#   ?SUB_EXPR: child operator

%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    ?SUB_EXPR(State).
