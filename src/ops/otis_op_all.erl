-module(otis_op_all).

-include_lib("yamerl/include/yamerl_nodes.hrl").

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

create(Ruleset, Keyword, #yamerl_seq{entries = Sub_Exprs}) ->
    [#op_all{
        subexprs = otis_conf:create_exprs(Ruleset, Sub_Exprs),
        line     = yamerl_constr:node_line(Keyword),
        col      = yamerl_constr:node_column(Keyword)
      }];
create(Ruleset, Keyword, Node) ->
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Expected a sequence of sub-expressions.~n",
      [Line, Col]).

%% -------------------------------------------------------------------
%% Code generation
%% -------------------------------------------------------------------

gen_code(#code{cursor = Cursor, ruleset = Ruleset} = Code,
  #op_all{subexprs = Sub_Exprs, line = Line, col = Col} = Expr, Next_Fun) ->
    Tpl = try
        otis_tpl:read("all.erl")
    catch
        throw:invalid_template ->
            ?ERROR("Unsupported expression: ~p~n", [Expr]),
            throw(invalid_expression)
    end,
    %% Generate subexpressions.
    Cursor1 = [all | Cursor],
    Code1   = Code#code{
      cursor = Cursor1
    },
    {Sub_Expr_Fun, Sub_Exprs_Text} = otis_tpl:gen_exprs(Code1, Sub_Exprs,
      Next_Fun),
    %% Prepare template variables.
    Expr_Fun = otis_tpl:fun_name(Cursor1),
    Tpl_Vars = [
      {"FUNCTION",  Expr_Fun},
      {"SUB_EXPR",  Sub_Expr_Fun},
      {"SRC_FILE",  Ruleset#ruleset.file},
      {"SRC_LINE",  io_lib:format("~b", [Line])},
      {"SRC_COL",   io_lib:format("~b", [Col])}
    ],
    Text = otis_tpl:expand(Tpl, Tpl_Vars),
    {Expr_Fun, [Text | Sub_Exprs_Text]}.
