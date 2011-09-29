-module(otis_op_any).

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
    [#op_any{
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
  #op_any{subexprs = Sub_Exprs, line = Line, col = Col} = Expr, Next_Fun) ->
    Tpl1 = try
        otis_tpl:read("any_last.erl")
    catch
        throw:invalid_template ->
            ?ERROR("Unsupported expression: ~p~n", [Expr]),
            throw(invalid_expression)
    end,
    %% Generate last subexpression.
    [Last_Sub | Other_Subs] = lists:reverse(Sub_Exprs),
    Exprs_Count = length(Sub_Exprs),
    Cursor1 = [{any, Exprs_Count} | Cursor],
    Code1   = Code#code{
      cursor = Cursor1
    },
    {Last_Sub_Fun, Last_Sub_Text} = otis_tpl:gen_exprs(Code1, [Last_Sub],
      Next_Fun),
    %% Prepare template variables.
    Expr_Fun  = otis_tpl:fun_name(Cursor1),
    Tpl1_Vars = [
      {"FUNCTION",  Expr_Fun},
      {"SUB_EXPR",  Last_Sub_Fun},
      {"SRC_FILE",  Ruleset#ruleset.file},
      {"SRC_LINE",  io_lib:format("~b", [Line])},
      {"SRC_COL",   io_lib:format("~b", [Col])}
    ],
    Text = otis_tpl:expand(Tpl1, Tpl1_Vars),
    %% Load template for other subexpressions.
    Tpl2 = try
        otis_tpl:read("any.erl")
    catch
        throw:invalid_template ->
            ?ERROR("Unsupported expression: ~p~n", [Expr]),
            throw(invalid_expression)
    end,
    gen_code2(Code, Cursor, Line, Col, Other_Subs, Exprs_Count - 1,
      Expr_Fun, Next_Fun, Tpl2, [Text | Last_Sub_Text]).

gen_code2(#code{ruleset = Ruleset} = Code,
  Cursor, Line, Col, [Sub | Rest], I, Or_Fun, Next_Fun, Tpl, Texts) ->
    Cursor1 = [{any, I} | Cursor],
    Code1   = Code#code{
      cursor = Cursor1
    },
    {Sub_Fun, Sub_Text} = otis_tpl:gen_exprs(Code1, [Sub],
      Next_Fun),
    %% Prepare template variables.
    Expr_Fun  = otis_tpl:fun_name(Cursor1),
    Tpl_Vars = [
      {"FUNCTION",  Expr_Fun},
      {"SUB_EXPR",  Sub_Fun},
      {"OR",        Or_Fun},
      {"SRC_FILE",  Ruleset#ruleset.file},
      {"SRC_LINE",  io_lib:format("~b", [Line])},
      {"SRC_COL",   io_lib:format("~b", [Col])}
    ],
    Text = otis_tpl:expand(Tpl, Tpl_Vars),
    gen_code2(Code, Cursor, Line, Col, Rest, I - 1, Expr_Fun, Next_Fun, Tpl,
      [Text] ++ Sub_Text ++ Texts);
gen_code2(_, _, _, _, [], _, Next_Fun, _, _, Texts) ->
    {Next_Fun, Texts}.
