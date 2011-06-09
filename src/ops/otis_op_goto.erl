-module(otis_op_goto).

-include_lib("yaml/include/yaml_nodes.hrl").

-include("otis.hrl").
-include("otis_codegen.hrl").

%% Public API.
-export([
    create/3,
    gen_code/3,
    target/1
  ]).

%% -------------------------------------------------------------------
%% String parsing.
%% -------------------------------------------------------------------

create(_, Keyword, #yaml_int{value = Target}) ->
    [#op_goto{
        target = Target,
        line   = yaml_repr:node_line(Keyword),
        col    = yaml_repr:node_column(Keyword)
      }];
create(_, Keyword, #yaml_str{text = Target}) ->
    [#op_goto{
        target = Target,
        line   = yaml_repr:node_line(Keyword),
        col    = yaml_repr:node_column(Keyword)
      }];
create(Ruleset, Keyword, Node) ->
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Expected a name or number of a rule or one of the~n"
      "  following keywords: FIRST, PREVIOUS, NEXT, LAST, STOP.~n",
      [Line, Col]).

%% -------------------------------------------------------------------
%% Code generation
%% -------------------------------------------------------------------

gen_code(#code{cursor = Cursor, ruleset = Ruleset} = Code,
  #op_goto{target = Target, line = Line, col = Col} = Expr, _) ->
    Tpl = try
        otis_tpl:read("goto.erl")
    catch
        throw:invalid_template ->
            ?ERROR("Unsupported expression: ~p~n", [Expr]),
            throw(invalid_expression)
    end,
    %% Prepare template variables.
    Expr_Fun = otis_tpl:fun_name([goto | Cursor]),
    Target1  = otis_var:expand_consts(Code, Target),
    Tpl_Vars = [
      {"FUNCTION", Expr_Fun},
      {"TARGET",   io_lib:format("~p", [Target1])},
      {"SRC_FILE", Ruleset#ruleset.file},
      {"SRC_LINE", io_lib:format("~b", [Line])},
      {"SRC_COL",  io_lib:format("~b", [Col])}
    ],
    Text = otis_tpl:expand(Tpl, Tpl_Vars),
    {Expr_Fun, [Text]}.

%% -------------------------------------------------------------------
%% Used by templates
%% -------------------------------------------------------------------

target("FIRST") ->
    first;
target("PREVIOUS") ->
    previous;
target("NEXT") ->
    next;
target("LAST") ->
    last;
target("STOP") ->
    stop;
target(Name) ->
    Name.
