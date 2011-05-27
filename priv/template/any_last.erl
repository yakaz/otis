%%# Operator "any".
%%#
%%# This template is used for the last child operator, instead of
%%# "any.erl". The ?NEXT operator is given to ?SUB_EXPR.
%%#
%%# Template variables:
%%#   ?FUNCTION: function name
%%#   ?SUB_EXPR: child operator

%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    ?SUB_EXPR(State).
