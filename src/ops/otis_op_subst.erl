-module(otis_op_subst).

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
    query_param/5,
    header/5,
    cookie/5
  ]).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

create(Ruleset, Keyword, #yaml_map{pairs = Args}) ->
    create2(Ruleset, Keyword, Args, #op_subst{});
create(Ruleset, Keyword, Node) ->
    Line = yaml_constr:node_line(Node),
    Col  = yaml_constr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Expected a map of the form \"variable: regex\".~n",
      [Line, Col]).

create2(Ruleset, Keyword,
  [{#yaml_str{text = "value"}, #yaml_str{text = Value}} | Rest],
  #op_subst{value = undefined} = Op) ->
    Value1 = otis_var:parse(Value),
    Op1 = Op#op_subst{
      value = Value1
    },
    create2(Ruleset, Keyword, Rest, Op1);
create2(Ruleset, Keyword,
  [{#yaml_str{text = "flags"}, #yaml_str{text = Flags} = Node} | Rest],
  #op_subst{match_flags = []} = Op) ->
    {Compile_Flags, Match_Flags} = otis_op_match:create_flags(Ruleset,
      Keyword, Node, Flags),
    Op1 = Op#op_subst{
      compile_flags = Compile_Flags,
      match_flags   = Match_Flags
    },
    create2(Ruleset, Keyword, Rest, Op1);
create2(Ruleset, Keyword,
  [{#yaml_str{text = Var} = Node, #yaml_str{text = Value}} | Rest],
  #op_subst{var = undefined} = Op) ->
    case otis_var:parse(Var) of
        #var{} = Var1 ->
            Value1 = otis_var:parse(Value),
            Op1 = Op#op_subst{
              var   = Var1,
              regex = Value1
            },
            create2(Ruleset, Keyword, Rest, Op1);
        _ ->
            Line = yaml_constr:node_line(Node),
            Col  = yaml_constr:node_column(Node),
            otis_conf:format_error(Ruleset, Keyword,
              "~b:~b: Left operand must be a variable.~n",
              [Line, Col])
    end;
create2(_, Keyword, [],
  #op_subst{var = Var, regex = Regex, value = Value, match_flags = Match} = Op)
  when Var /= undefined andalso Regex /= undefined andalso Value /= undefined ->
    Op1 = Op#op_subst{
      match_flags = [{return, list} | Match],
      line        = yaml_constr:node_line(Keyword),
      col         = yaml_constr:node_column(Keyword)
    },
    [Op1];
create2(Ruleset, Keyword,
  [{#yaml_str{text = "value"} = Attr, Node} | _],
  #op_subst{value = Value}) ->
    case Value of
        undefined ->
            Line = yaml_constr:node_line(Attr),
            Col  = yaml_constr:node_column(Attr),
            otis_conf:format_error(Ruleset, Keyword,
              "~b:~b: Only one \"value\" attribute allowed.~n",
              [Line, Col]);
        _ ->
            Line = yaml_constr:node_line(Node),
            Col  = yaml_constr:node_column(Node),
            otis_conf:format_error(Ruleset, Keyword,
              "~b:~b: \"value\" must be a scalar.~n",
              [Line, Col])
    end;
create2(Ruleset, Keyword,
  [{#yaml_str{text = "flags"} = Attr, Node} | _],
  #op_subst{match_flags = Flags}) ->
    case Flags of
        [] ->
            Line = yaml_constr:node_line(Attr),
            Col  = yaml_constr:node_column(Attr),
            otis_conf:format_error(Ruleset, Keyword,
              "~b:~b: Only one \"flags\" attribute allowed.~n",
              [Line, Col]);
        _ ->
            Line = yaml_constr:node_line(Node),
            Col  = yaml_constr:node_column(Node),
            otis_conf:format_error(Ruleset, Keyword,
              "~b:~b: \"flags\" must be a string listing the flags.~n",
              [Line, Col])
    end;
create2(Ruleset, Keyword,
  [{#yaml_str{} = Attr, Node} | _], #op_subst{var = Var}) ->
    case Var of
        undefined ->
            Line = yaml_constr:node_line(Attr),
            Col  = yaml_constr:node_column(Attr),
            otis_conf:format_error(Ruleset, Keyword,
              "~b:~b: Only one variable allowed.~n",
              [Line, Col]);
        _ ->
            Line = yaml_constr:node_line(Node),
            Col  = yaml_constr:node_column(Node),
            otis_conf:format_error(Ruleset, Keyword,
              "~b:~b: The variable must be associated to a regex (string).~n",
              [Line, Col])
    end;
create2(Ruleset, Keyword, [{Attr, _} | _], _) ->
    Line = yaml_constr:node_line(Attr),
    Col  = yaml_constr:node_column(Attr),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Unsupported attribute.~n", [Line, Col]);
create2(Ruleset, Keyword, [], _) ->
    otis_conf:format_error(Ruleset, Keyword,
      "Missing attribute(s).~n", []).

%% -------------------------------------------------------------------
%% Code generation
%% -------------------------------------------------------------------

gen_code(#code{cursor = Cursor, ruleset = Ruleset} = Code,
  #op_subst{line = Line, col = Col} = Expr, Next_Fun) ->
    %% Generate the expression code.
    Tpl = try
        otis_tpl:read(get_tpl_name(Expr))
    catch
        throw:invalid_template ->
            ?ERROR("Unsupported expression: ~p~n", [Expr]),
            throw(invalid_expression)
    end,
    %% Prepare template variables.
    Expr_Fun = otis_tpl:fun_name([subst | Cursor]),
    Tpl_Vars = get_tpl_vars(Code, Expr, [
        {"FUNCTION",  Expr_Fun},
        {"NEXT_EXPR", Next_Fun},
        {"SRC_FILE",  Ruleset#ruleset.file},
        {"SRC_LINE",  io_lib:format("~b", [Line])},
        {"SRC_COL",   io_lib:format("~b", [Col])}
      ]),
    Text = otis_tpl:expand(Tpl, Tpl_Vars),
    {Expr_Fun, [Text]}.

get_tpl_name(#op_subst{var = Var}) ->
    string:join(
      ["subst", otis_var:domain(Var)],
      "_") ++ ".erl".

get_tpl_vars(Code,
  #op_subst{var = #var{prefix = undefined} = Var} = Expr, Vars) ->
    Var1   = otis_var:expand_consts(Code, Var),
    Member = otis_var:state_member(Var),
    get_tpl_vars2(Code, Expr, [
        {"MEMBER", Member},
        {"VAR",    io_lib:format("~p", [Var1])}
        | Vars
      ]);
get_tpl_vars(Code,
  #op_subst{var = #var{name = Name}} = Expr, Vars) ->
    Name1 = otis_var:expand_consts(Code, Name),
    get_tpl_vars2(Code, Expr, [
        {"NAME", io_lib:format("~p", [Name1])}
        | Vars
      ]).

get_tpl_vars2(Code,
  #op_subst{regex = Regex, compile_flags = Compile_Flags,
    match_flags = Match_Flags, value = Value},
  Vars) ->
    Regex1 = otis_var:expand_consts(Code, Regex),
    Value1 = otis_var:expand_consts(Code, Value),
    Vars1  = case otis_var:is_var(Regex1) of
        true ->
            [
              {"REGEX_S", "see below"},
              {"FLAGS_S", "see below"},
              {"REGEX", io_lib:format("~p", [Regex1])},
              {"FLAGS", io_lib:format("~p", [Compile_Flags ++ Match_Flags])}
              | Vars
            ];
        false ->
            case re:compile(Regex1, Compile_Flags) of
                {ok, Regex2} ->
                    [
                      {"REGEX_S", io_lib:format("~s", [Regex1])},
                      {"FLAGS_S", io_lib:format("~w", [Compile_Flags])},
                      {"REGEX",   io_lib:format("~p", [Regex2])},
                      {"FLAGS",   io_lib:format("~p", [Match_Flags])}
                      | Vars
                    ];
                {error, Reason} ->
                    ?ERROR("Invalid regex: ~p~n", [Reason]),
                    throw(invalid_op_subst)
            end
    end,
    [
      {"VALUE",    io_lib:format("~p", [Value1])}
      | Vars1
    ].

%% -------------------------------------------------------------------
%% Used by templates
%% -------------------------------------------------------------------

query_param(#state{query_parsed = false} = State, Name, Regex, Flags,
  Value) ->
    State1 = otis_utils:parse_query(State),
    query_param(State1, Name, Regex, Flags, Value);
query_param(#state{query_str = Query} = State, Name, Regex, Flags,
  Value) ->
    Query1 = loop(Name, Regex, Flags, Value, Query, []),
    State#state{
      query_str = Query1
    }.

header(#state{headers = Headers} = State, Name, Regex, Flags,
  Value) ->
    Name1    = string:to_lower(Name),
    Headers1 = loop(Name1, Regex, Flags, Value, Headers, []),
    State#state{
      headers = Headers1
    }.

cookie(#state{cookies = undefined} = State, Name, Regex, Flags,
  Value) ->
    State1 = otis_utils:parse_cookies(State),
    cookie(State1, Name, Regex, Flags, Value);
cookie(#state{cookies = Cookies} = State, Name, Regex, Flags,
  Value) ->
    Cookies1 = loop(Name, Regex, Flags, Value, Cookies, []),
    State#state{
      query_str = Cookies1
    }.

loop(Name, Regex, Flags, Value,
  [{Name, Old_Value, _, _} | Rest], List) when Old_Value /= undefined ->
    Value1 = re:replace(Old_Value, Regex, Value, Flags),
    Item1 = {Name, Value1, undefined, undefined},
    loop(Name, Regex, Flags, Value, Rest,
      [Item1 | List]);
loop(Name, Regex, Flags, Value,
  [{Name, undefined, Type_Mod, Value_C} | Rest], List) ->
    Value_S = Type_Mod:to_string(Value_C),
    Value1 = re:replace(Value_S, Regex, Value, Flags),
    Item1 = {Name, Value1, undefined, undefined},
    loop(Name, Regex, Flags, Value, Rest,
      [Item1 | List]);
loop(Name, Regex, Flags, Value, [Item | Rest], List) ->
    loop(Name, Regex, Flags, Value, Rest, [Item | List]);
loop(_, _, _, _, [], List) ->
    List.
