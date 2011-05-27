%%# Operator "goto".
%%#
%%# Jump to another rule. The selected rule depends on goto's argument:
%%#     o  FIRST:    first rule of the configuration file
%%#     o  PREVIOUS: previous rule in the configuration file
%%#     o  NEXT:     next rule in the configuration file
%%#     o  LAST:     last rule in the configuration file
%%#     o  "Rule":   name of the rule to jump to
%%#     o  3:        rule #3
%%#
%%# Template variables:
%%#   ?FUNCTION:  function name
%%#   ?TARGET:    first | previous | next | last | Rule name | Rule position

%% Source: ?SRC_FILE:?SRC_LINE,?SRC_COL

?FUNCTION(State) ->
    {State1, Name} = otis_var:expand(State, ?TARGET),
    case otis_op_goto:target(Name) of
        stop   -> otis_utils:stop(State1);
        Target -> throw({Target, State1})
    end.
