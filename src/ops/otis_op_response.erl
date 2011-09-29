-module(otis_op_response).

-include_lib("yaml/include/yaml_nodes.hrl").

-include("otis.hrl").
-include("otis_codegen.hrl").

%% Public API.
-export([
    create/3,
    gen_code/3,
    set_location/1
  ]).

-define(VALID_HTTP_CODE(C), (
    (C >= 100 andalso C =< 102) orelse C == 122 orelse
    (C >= 200 andalso C =< 207) orelse C == 226 orelse
    (C >= 301 andalso C =< 307) orelse
    (C >= 400 andalso C =< 418) orelse (C >= 422 andalso C =< 426) orelse
    C == 444 orelse C == 449 orelse C == 450 orelse C == 499 orelse
    (C >= 500 andalso C =< 507) orelse C == 509 orelse C == 510
  )).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

create(Ruleset, Keyword, #yaml_int{value = Code}) when Code > 0 ->
    %% The user only specified an HTTP code.
    Op = #op_response{
      code = Code
    },
    create3(Ruleset, Keyword, Op);
create(Ruleset, Keyword, #yaml_map{pairs = Pairs}) ->
    %% The user specified one or several attributes.
    create2(Ruleset, Keyword, #op_response{}, Pairs);
create(Ruleset, Keyword, #yaml_int{} = Node) ->
    Line = yaml_constr:node_line(Node),
    Col  = yaml_constr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: An HTTP code must be greater than zero.~n",
      [Line, Col]);
create(Ruleset, Keyword, Node) ->
    Line = yaml_constr:node_line(Node),
    Col  = yaml_constr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Expected an HTTP code or "
      "a map of the form \"attribute: value\".~n",
      [Line, Col]).

create2(Ruleset, Keyword, Op,
  [{#yaml_str{text = "code"}, #yaml_int{value = Code}} | Rest])
  when ?VALID_HTTP_CODE(Code) ->
    Op1 = Op#op_response{
      code = Code
    },
    create2(Ruleset, Keyword, Op1, Rest);
create2(Ruleset, Keyword, Op,
  [{#yaml_str{text = "reason"}, #yaml_str{text = Reason}} | Rest]) ->
    Op1 = Op#op_response{
      reason = Reason
    },
    create2(Ruleset, Keyword, Op1, Rest);
create2(Ruleset, Keyword, Op, []) ->
    create3(Ruleset, Keyword, Op);
create2(Ruleset, Keyword, _,
  [{#yaml_str{text = "code"}, #yaml_int{} = Node} | _]) ->
    Line = yaml_constr:node_line(Node),
    Col  = yaml_constr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: \"code\" must be a valid HTTP code.~n",
      [Line, Col]);
create2(Ruleset, Keyword, _, [{Attr, _} | _]) ->
    Line = yaml_constr:node_line(Attr),
    Col  = yaml_constr:node_column(Attr),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Unsupported attribute.~n",
      [Line, Col]).

create3(Ruleset, Keyword, #op_response{code = Code, reason = undefined} = Op) ->
    Op1 = Op#op_response{
      reason = httpd_util:reason_phrase(Code)
    },
    create3(Ruleset, Keyword, Op1);
create3(_, Keyword, Op) ->
    Op1 = Op#op_response{
      line = yaml_constr:node_line(Keyword),
      col  = yaml_constr:node_column(Keyword)
    },
    [Op1].

%% -------------------------------------------------------------------
%% Code generation
%% -------------------------------------------------------------------

gen_code(#code{cursor = Cursor, ruleset = Ruleset} = Code,
  #op_response{code = Status_Code, reason = Reason,
  line = Line, col = Col} = Expr, _) ->
    Tpl = try
        otis_tpl:read(get_tpl_name(Expr))
    catch
        throw:invalid_template ->
            ?ERROR("Unsupported expression: ~p~n", [Expr]),
            throw(invalid_expression)
    end,
    %% Prepare template variables.
    Expr_Fun = otis_tpl:fun_name([response | Cursor]),
    Reason1  = otis_var:expand_consts(Code, Reason),
    Tpl_Vars = [
      {"FUNCTION",  Expr_Fun},
      {"CODE",      io_lib:format("~b", [Status_Code])},
      {"REASON",    io_lib:format("~p", [Reason1])},
      {"SRC_FILE",  Ruleset#ruleset.file},
      {"SRC_LINE",  io_lib:format("~b", [Line])},
      {"SRC_COL",   io_lib:format("~b", [Col])}
    ],
    Text = otis_tpl:expand(Tpl, Tpl_Vars),
    {Expr_Fun, [Text]}.

get_tpl_name(#op_response{code = Code})
  when Code >= 301 andalso Code =< 307 andalso Code /= 304 ->
    "redirect.erl";
get_tpl_name(_) ->
    "response.erl".

%% -------------------------------------------------------------------
%% Used by templates
%% -------------------------------------------------------------------

set_location(State) ->
    case otis_var:get_rheader(State, "location") of
        {State1, undefined} ->
            %% We construct a Location header based on the state
            %% of the rewritten request.
            {State2, URI} = otis_utils:format_uri(State1),
            otis_var:set_rheader(State2, "location", URI);
        {State1, _} ->
            State1
    end.
