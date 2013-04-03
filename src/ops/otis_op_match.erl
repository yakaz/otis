-module(otis_op_match).

-include_lib("yaml/include/yaml_nodes.hrl").

-include("otis.hrl").
-include("otis_codegen.hrl").

%% Public API.
-export([
    create/3,
    gen_code/3,
    create_flags/4
  ]).

%% Used by templates.
-export([
    handle_captures/3,
    query_param/5,
    header/5,
    cookie/5
  ]).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

create(Ruleset, Keyword, #yaml_map{pairs = Args}) ->
    create2(Ruleset, Keyword, Args, #op_match{});
create(Ruleset, Keyword, Node) ->
    Line = yaml_constr:node_line(Node),
    Col  = yaml_constr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Expected a map of the form \"variable: regex\".~n",
      [Line, Col]).

create2(Ruleset, Keyword,
  [{#yaml_str{text = "captures"}, #yaml_seq{entries = Captures}} | Rest],
  #op_match{captures = []} = Op) ->
    Captures1 = create_captures(Ruleset, Keyword, Captures, []),
    Op1 = Op#op_match{
      captures = Captures1
    },
    create2(Ruleset, Keyword, Rest, Op1);
create2(Ruleset, Keyword,
  [{#yaml_str{text = "flags"}, #yaml_str{text = Flags} = Node} | Rest],
  #op_match{match_flags = []} = Op) ->
    {Compile_Flags, Match_Flags} = create_flags(Ruleset, Keyword,
      Node, Flags),
    Op1 = Op#op_match{
      compile_flags = Compile_Flags,
      match_flags   = Match_Flags
    },
    create2(Ruleset, Keyword, Rest, Op1);
create2(Ruleset, Keyword,
  [{#yaml_str{text = Var} = Node, #yaml_str{text = Value}} | Rest],
  #op_match{var = undefined} = Op) ->
    case otis_var:parse(Var) of
        #var{} = Var1 ->
            Line   = yaml_constr:node_line(Keyword),
            Col    = yaml_constr:node_column(Keyword),
            Value1 = otis_var:parse(Value),
            Op1 = Op#op_match{
              var   = Var1,
              regex = Value1,
              line  = Line,
              col   = Col
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
  #op_match{var = Var, regex = Regex, match_flags = Match} = Op)
  when Var /= undefined andalso Regex /= undefined ->
    Op1 = Op#op_match{
      match_flags = [{capture, all_but_first, list} | Match],
      line        = yaml_constr:node_line(Keyword),
      col         = yaml_constr:node_column(Keyword)
    },
    [Op1];
create2(Ruleset, Keyword,
  [{#yaml_str{text = "captures"} = Attr, Node} | _],
  #op_match{captures = Captures}) ->
    case Captures of
        [] ->
            Line = yaml_constr:node_line(Attr),
            Col  = yaml_constr:node_column(Attr),
            otis_conf:format_error(Ruleset, Keyword,
              "~b:~b: Only one \"captures\" attribute allowed.~n",
              [Line, Col]);
        _ ->
            Line = yaml_constr:node_line(Node),
            Col  = yaml_constr:node_column(Node),
            otis_conf:format_error(Ruleset, Keyword,
              "~b:~b: \"captures\" must be a list of variable.~n",
              [Line, Col])
    end;
create2(Ruleset, Keyword,
  [{#yaml_str{text = "flags"} = Attr, Node} | _],
  #op_match{match_flags = Flags}) ->
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
  [{#yaml_str{} = Attr, Node} | _], #op_match{var = Var}) ->
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

create_captures(Ruleset, Keyword, [#yaml_str{text = Var} = Capt | Rest],
  Result) ->
    case otis_var:parse(Var) of
        #var{} = Var1 ->
            create_captures(Ruleset, Keyword, Rest, [Var1 | Result]);
        _ ->
            Line = yaml_constr:node_line(Capt),
            Col  = yaml_constr:node_column(Capt),
            otis_conf:format_error(Ruleset, Keyword,
              "~b:~b: Expected a variable.~n",
              [Line, Col])
    end;
create_captures(_, _, [], Result) ->
    lists:reverse(Result);
create_captures(Ruleset, Keyword, [Capt | _], _) ->
    Line = yaml_constr:node_line(Capt),
    Col  = yaml_constr:node_column(Capt),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Expected a variable.~n",
      [Line, Col]).

create_flags(Ruleset, Keyword, Node, Flags) ->
    create_flags2(Ruleset, Keyword, Node, Flags, [], []).

create_flags2(Ruleset, Keyword, Node, [$g | Rest], Compile, Match) ->
    create_flags2(Ruleset, Keyword, Node, Rest, Compile, [global | Match]);
create_flags2(Ruleset, Keyword, Node, [$i | Rest], Compile, Match) ->
    create_flags2(Ruleset, Keyword, Node, Rest, [caseless | Compile], Match);
create_flags2(_, _, _, [], Compile, Match) ->
    {
      lists:reverse(Compile),
      lists:reverse(Match)
    };
create_flags2(Ruleset, Keyword, Node, [Flag | _], _, _) ->
    Line = yaml_constr:node_line(Node),
    Col  = yaml_constr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Invalid regex flag '~p'~n", [Line, Col, Flag]).

%% -------------------------------------------------------------------
%% Code generation
%% -------------------------------------------------------------------

gen_code(#code{cursor = Cursor, ruleset = Ruleset} = Code,
  #op_match{line = Line, col = Col} = Expr, Next_Fun) ->
    %% Generate the expression code.
    Tpl = try
        otis_tpl:read(get_tpl_name(Expr))
    catch
        throw:invalid_template ->
            ?ERROR("Unsupported expression: ~p~n", [Expr]),
            throw(invalid_expression)
    end,
    %% Prepare template variables.
    Expr_Fun = otis_tpl:fun_name([match | Cursor]),
    Tpl_Vars = get_tpl_vars(Code, Expr, [
        {"FUNCTION",  Expr_Fun},
        {"NEXT_EXPR", Next_Fun},
        {"SRC_FILE",  Ruleset#ruleset.file},
        {"SRC_LINE",  io_lib:format("~b", [Line])},
        {"SRC_COL",   io_lib:format("~b", [Col])}
      ]),
    Text = otis_tpl:expand(Tpl, Tpl_Vars),
    {Expr_Fun, [Text]}.

get_tpl_name(#op_match{var = Var}) ->
    string:join(
      ["match", otis_var:domain(Var)],
      "_") ++ ".erl".

get_tpl_vars(Code,
  #op_match{var = #var{prefix = undefined} = Var} = Expr, Vars) ->
    Var1   = otis_var:expand_consts(Code, Var),
    Member = otis_var:state_member(Var),
    get_tpl_vars2(Code, Expr, [
        {"MEMBER", Member},
        {"VAR",    io_lib:format("~p", [Var1])}
        | Vars
      ]);
get_tpl_vars(Code,
  #op_match{var = #var{name = Name}} = Expr, Vars) ->
    Name1 = otis_var:expand_consts(Code, Name),
    get_tpl_vars2(Code, Expr, [
        {"NAME", io_lib:format("~p", [Name1])}
        | Vars
      ]).

get_tpl_vars2(Code,
  #op_match{regex = Regex, compile_flags = Compile_Flags,
    match_flags = Match_Flags, captures = Captures},
  Vars) ->
    Regex1 = otis_var:expand_consts(Code, Regex),
    Vars1  = case otis_var:is_var(Regex1) of
        true ->
            [
              {"REGEX_S", "see below"},
              {"FLAGS_S", "see below"},
              {"REGEX",   io_lib:format("~p", [Regex1])},
              {"FLAGS",   io_lib:format("~p", [Compile_Flags ++ Match_Flags])}
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
                    throw(invalid_op_match)
            end
    end,
    [
      {"CAPTURES", io_lib:format("~p", [Captures])}
      | Vars1
    ].

%% -------------------------------------------------------------------
%% Used by templates
%% -------------------------------------------------------------------

handle_captures(State, [Var | Rest1], [Value | Rest2]) ->
    State1 = otis_var:set(State, Var, Value),
    handle_captures(State1, Rest1, Rest2);
handle_captures(State, [Var | Rest1], []) ->
    State1 = otis_var:set(State, Var, ""),
    handle_captures(State1, Rest1, []);
handle_captures(State, [], Captured) ->
    case Captured of
        [] ->
            ok;
        _ ->
            ?WARNING("~s: ~b captured values not handled: ~p~n",
              [State#state.rule_name, length(Captured), Captured])
    end,
    State.

query_param(#state{query_parsed = false} = State, Name, Regex, Flags,
  Captures) ->
    State1 = otis_utils:parse_query(State),
    query_param(State1, Name, Regex, Flags, Captures);
query_param(#state{query_str = Query} = State, Name, Regex, Flags,
  Captures) ->
    case loop(Name, Regex, Flags, Query, [], false) of
        nomatch ->
            otis_utils:abort(State);
        {match, Captured} ->
            handle_captures(State, Captures, Captured);
        {match, Captured, Query1} ->
            State1 = State#state{
              query_str = Query1
            },
            handle_captures(State1, Captures, Captured)
    end.

header(#state{headers = Headers} = State, Name, Regex, Flags,
  Captures) ->
    Name1 = string:to_lower(Name),
    case loop(Name1, Regex, Flags, Headers, [], false) of
        nomatch ->
            otis_utils:abort(State);
        {match, Captured} ->
            handle_captures(State, Captures, Captured);
        {match, Captured, Headers1} ->
            State1 = State#state{
              headers = Headers1
            },
            handle_captures(State1, Captures, Captured)
    end.

cookie(#state{cookies = undefined} = State, Name, Regex, Flags,
  Captures) ->
    State1 = otis_utils:parse_cookies(State),
    cookie(State1, Name, Regex, Flags, Captures);
cookie(#state{cookies = Cookies} = State, Name, Regex, Flags,
  Captures) ->
    case loop(Name, Regex, Flags, Cookies, [], false) of
        nomatch ->
            otis_utils:abort(State);
        {match, Captured} ->
            handle_captures(State, Captures, Captured);
        {match, Captured, Cookies1} ->
            State1 = State#state{
              cookies = Cookies1
            },
            handle_captures(State1, Captures, Captured)
    end.

loop(Name, Regex, Flags,
  [{Name, Value, _, _} = Item | List_Tail],
  List_Head, Converted) when Value /= undefined ->
    %% Item's value is already a string.
    case re:run(Value, Regex, Flags) of
        {match, Captured} ->
            %% We have a match.
            loop2([Item | List_Head], List_Tail, Converted,
              Captured);
        nomatch ->
            %% No match, continue.
            loop(Name, Regex, Flags,  List_Tail,
              [Item | List_Head], Converted)
    end;
loop(Name, Regex, Flags,
  [{Name, undefined, Type_Mod, Value_C} | List_Tail],
  List_Head, _) ->
    %% Item's value must be converted from Type_Mod to string first.
    Value_S = Type_Mod:to_string(Value_C),
    Item1  = {Name, Value_S, Type_Mod, Value_C},
    case re:run(Value_S, Regex, Flags) of
        {match, Captured} ->
            %% We have a match.
            loop2([Item1 | List_Head], List_Tail, true,
              Captured);
        nomatch ->
            loop(Name, Regex, Flags,  List_Tail,
              [Item1 | List_Head], true)
    end;
loop(Name, Regex, Flags,  [Item | Rest],
  Converted, Conv_Flag) ->
    loop(Name, Regex, Flags,  Rest,
      [Item | Converted], Conv_Flag);
loop(_, _, _, [], _, _) ->
    nomatch.

loop2(_, _, false, Captured) ->
    {match, Captured};
loop2(List_Head, List_Tail, true, Captured) ->
    List1 = lists:reverse(List_Head) ++ List_Tail,
    {match, Captured, List1}.
