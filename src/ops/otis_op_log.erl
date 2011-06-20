-module(otis_op_log).

-include_lib("yaml/include/yaml_nodes.hrl").

-include("otis.hrl").
-include("otis_codegen.hrl").

%% Public API.
-export([
    create/3,
    gen_code/3
  ]).

%% Used by templates.
-export([
    convert_http_newlines/1
  ]).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

create(_, Keyword, #yaml_str{text = Format_String}) ->
    Prepared = otis_var:prepare_string(Format_String),
    [#op_log{
        format_str = Prepared,
        line       = yaml_repr:node_line(Keyword),
        col        = yaml_repr:node_column(Keyword)
      }];
create(Ruleset, Keyword, Node) ->
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Expected a message to log (string).~n",
      [Line, Col]).

%% -------------------------------------------------------------------
%% Code generation
%% -------------------------------------------------------------------

gen_code(#code{cursor = Cursor, ruleset = Ruleset} = Code,
  #op_log{format_str = Format, line = Line, col = Col} = Expr, Next_Fun) ->
    %% Generate the expression code.
    Tpl = try
        otis_tpl:read("log.erl")
    catch
        throw:invalid_template ->
            ?ERROR("Unsupported expression: ~p~n", [Expr]),
            throw(invalid_expression)
    end,
    %% Prepare template variables.
    Expr_Fun = otis_tpl:fun_name([log | Cursor]),
    Format1  = otis_var:expand_consts(Code, Format),
    Tpl_Vars = [
      {"FUNCTION",  Expr_Fun},
      {"FORMAT",    io_lib:format("~n      ~p~n    ", [Format1])},
      {"NEXT_EXPR", Next_Fun},
      {"SRC_FILE",  Ruleset#ruleset.file},
      {"SRC_LINE",  io_lib:format("~b", [Line])},
      {"SRC_COL",   io_lib:format("~b", [Col])}
    ],
    Text = otis_tpl:expand(Tpl, Tpl_Vars),
    {Expr_Fun, [Text]}.

%% -------------------------------------------------------------------
%% Used by templates
%% -------------------------------------------------------------------

convert_http_newlines(String) ->
    convert_http_newlines2(String, []).

convert_http_newlines2([$\r, $\n | Rest], Result) ->
    convert_http_newlines2(Rest, [$\n | Result]);
convert_http_newlines2([C | Rest], Result) ->
    convert_http_newlines2(Rest, [C | Result]);
convert_http_newlines2([], Result) ->
    lists:reverse(Result).
