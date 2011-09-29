-module(otis_op_not).

-include_lib("yaml/include/yaml_nodes.hrl").

-include("otis.hrl").
-include("otis_codegen.hrl").

%% Public API.
-export([
    create/3,
    gen_code/3
  ]).

%% -------------------------------------------------------------------
%% String parsing.
%% -------------------------------------------------------------------

create(Ruleset, Keyword, #yaml_seq{entries = Sub_Exprs}) ->
    [#op_not{
        subexprs = otis_conf:create_exprs(Ruleset, Sub_Exprs),
        line     = yaml_constr:node_line(Keyword),
        col      = yaml_constr:node_column(Keyword)
      }];
create(Ruleset, Keyword, Node) ->
    Line = yaml_constr:node_line(Node),
    Col  = yaml_constr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Expected a sequence of sub-expressions.~n",
      [Line, Col]).

%% -------------------------------------------------------------------
%% Code generation
%% -------------------------------------------------------------------

gen_code(#code{cursor = Cursor, ruleset = Ruleset} = Code,
  #op_not{subexprs = Sub_Exprs, line = Line, col = Col} = Expr, Next_Fun) ->
    Tpl = try
        otis_tpl:read("not.erl")
    catch
        throw:invalid_template ->
            ?ERROR("Unsupported expression: ~p~n", [Expr]),
            throw(invalid_expression)
    end,
    %% Generate subexpressions.
    Cursor1   = ['not' | Cursor],
    Expr_Fun  = otis_tpl:fun_name(Cursor1),
    Expr_Fun2 = Expr_Fun ++ "2",
    Code1     = Code#code{
      cursor = Cursor1
    },
    {Sub_Expr_Fun, Sub_Exprs_Text} = otis_tpl:gen_exprs(Code1, Sub_Exprs,
      Expr_Fun2),
    %% Prepare template variables.
    Tpl_Vars = [
      {"FUNCTION2", Expr_Fun2},
      {"FUNCTION",  Expr_Fun},
      {"SUB_EXPR",  Sub_Expr_Fun},
      {"NEXT_EXPR", Next_Fun},
      {"SRC_FILE",  Ruleset#ruleset.file},
      {"SRC_LINE",  io_lib:format("~b", [Line])},
      {"SRC_COL",   io_lib:format("~b", [Col])}
    ],
    Text = otis_tpl:expand(Tpl, Tpl_Vars),
    {Expr_Fun, [Text | Sub_Exprs_Text]}.
