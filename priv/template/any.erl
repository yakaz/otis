%%# Operator "any".
%%#
%%# Call "child" operators until one succeeds, or abort. The ?NEXT
%%# operator is given to ?SUB_EXPR.
%%#
%%# Template variables:
%%#   ?FUNCTION: function name
%%#   ?SUB_EXPR: child operator
%%#   ?OR:       next "any" expression

%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    try
        ?SUB_EXPR(State)
    catch
        throw:{abort, _} -> ?OR(State)
    end.
