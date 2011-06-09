-module(otis_op_eq).

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
    query_param/3,
    query_param/4,
    header/3,
    header/4
  ]).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

create(Ruleset, Keyword,
  #yaml_map{pairs = [{#yaml_str{text = Var} = Node, Value}]}) ->
    case otis_var:prepare_string(Var) of
        [#var{} = Var1] ->
            create2(Ruleset, Keyword, Var1, Value);
        _ ->
            Line = yaml_repr:node_line(Node),
            Col  = yaml_repr:node_column(Node),
            otis_conf:format_error(Ruleset, Keyword,
              "~b:~b: Left operand must be a variable.~n",
              [Line, Col])
    end;
create(Ruleset, Keyword,
  #yaml_map{pairs = [{Node, _}]}) ->
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Left operand must be a variable.~n",
      [Line, Col]);
create(Ruleset, Keyword, Node) ->
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Expected a map of the \"variable: value\".~n",
      [Line, Col]).

create2(Ruleset, Keyword, Var, #yaml_str{text = Value}) ->
    %% String exact matching.
    Value1 = otis_var:prepare_string(Value),
    create3(Ruleset, Keyword, Var, Value1, string);
create2(Ruleset, Keyword, Var, #yaml_int{value = Value}) ->
    %% Integer exact matching.
    create3(Ruleset, Keyword, Var, Value, int);
create2(Ruleset, Keyword, _, Node) ->
    %% Unsupported type.
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Type of right operand is unsupported~n", [Line, Col]).

create3(_, Keyword, Var, Value, Type) ->
    Type1 = case otis_var:expected_type(Var) of
        undefined -> Type;
        T         -> T
    end,
    [#op_eq{
        var   = Var,
        value = Value,
        type  = Type1,
        line  = yaml_repr:node_line(Keyword),
        col   = yaml_repr:node_column(Keyword)
      }].

%% -------------------------------------------------------------------
%% Code generation
%% -------------------------------------------------------------------

gen_code(#code{cursor = Cursor, ruleset = Ruleset} = Code,
  #op_eq{line = Line, col = Col} = Expr, Next_Fun) ->
    %% Generate the expression code.
    Tpl = try
        otis_tpl:read(get_tpl_name(Expr))
    catch
        throw:invalid_template ->
            ?ERROR("Unsupported expression: ~p~n", [Expr]),
            throw(invalid_expression)
    end,
    %% Prepare template variables.
    Expr_Fun = otis_tpl:fun_name([eq | Cursor]),
    Tpl_Vars = get_tpl_vars(Code, Expr, [
        {"FUNCTION",  Expr_Fun},
        {"NEXT_EXPR", Next_Fun},
        {"SRC_FILE",  Ruleset#ruleset.file},
        {"SRC_LINE",  io_lib:format("~b", [Line])},
        {"SRC_COL",   io_lib:format("~b", [Col])}
      ]),
    Text = otis_tpl:expand(Tpl, Tpl_Vars),
    {Expr_Fun, [Text]}.

get_tpl_name(#op_eq{var = #var{prefix = Prefix} = Var})
  when Prefix == "query" orelse Prefix == "header" ->
    string:join(["eq", otis_var:domain(Var)], "_") ++ ".erl";
get_tpl_name(#op_eq{var = Var, type = Type}) ->
    case otis_var:domain(Var) of
        "user" -> "eq_user.erl";
        Domain -> string:join(["eq", Domain, atom_to_list(Type)], "_") ++ ".erl"
    end.

get_tpl_vars(Code,
  #op_eq{var = #var{prefix = undefined} = Var, value = Value, type = Type},
  Vars) ->
    Var1     = otis_var:expand_consts(Code, Var),
    Value1   = otis_var:expand_consts(Code, Value),
    Member   = otis_var:state_member(Var),
    Type_Mod = otis_utils:type_mod(Type),
    [
      {"MEMBER",   Member},
      {"VAR",      io_lib:format("~p", [Var1])},
      {"VALUE",    io_lib:format("~p", [Value1])},
      {"TYPE_MOD", io_lib:format("~s", [Type_Mod])}
      | Vars
    ];
get_tpl_vars(Code,
  #op_eq{var = #var{prefix = Prefix, name = Name}, value = Value, type = Type},
  Vars) when Prefix == "query" orelse Prefix == "header" ->
    Name1    = otis_var:expand_consts(Code, Name),
    Value1   = otis_var:expand_consts(Code, Value),
    Type_Mod = otis_utils:type_mod(Type),
    [
      {"NAME",     io_lib:format("~p", [Name1])},
      {"VALUE",    io_lib:format("~p", [Value1])},
      {"TYPE_MOD", io_lib:format("~s", [Type_Mod])}
      | Vars
    ].

%% -------------------------------------------------------------------
%% Used by templates
%% -------------------------------------------------------------------

query_param(State, Name, Value) ->
    query_param(State, Name, Value, undefined).

query_param(#state{query_parsed = false} = State, Name, Value, Type_Mod) ->
    State1 = otis_utils:parse_query(State),
    query_param(State1, Name, Value, Type_Mod);
query_param(#state{query_str = Query} = State, Name, Value, Type_Mod) ->
    Ret = loop(Name, Value, Type_Mod, Query, [], false),
    {Result, State1} = case Ret of
        {R, Query1} ->
            S1 = State#state{
              query_str = Query1
            },
            {R, S1};
        R ->
            {R, State}
    end,
    case Result of
        match   -> State1;
        nomatch -> otis_utils:abort(State1)
    end.

header(State, Name, Value) ->
    header(State, Name, Value, undefined).

header(#state{headers = Headers} = State, Name, Value, Type_Mod) ->
    Name1 = string:to_lower(Name),
    Ret   = loop(Name1, Value, Type_Mod, Headers, [], false),
    {Result, State1} = case Ret of
        {R, Headers1} ->
            S1 = State#state{
              headers = Headers1
            },
            {R, S1};
        R ->
            {R, State}
    end,
    case Result of
        match   -> State1;
        nomatch -> otis_utils:abort(State1)
    end.

loop(Name, Value, undefined,
  [{Name, Value, _, _} | _] = List_Tail,
  List_Head, Converted) when Value /= undefined ->
    %% Item's value is already a string and we have a match.
    loop2(List_Head, List_Tail, Converted);
loop(Name, Value, undefined,
  [{Name, undefined, Type_Mod, Value_C} | List_Tail], List_Head, _) ->
    %% Item's value must be converted from Type_Mod to string first.
    Value_S = Type_Mod:to_string(Value_C),
    Item1   = {Name, Value_S, Type_Mod, Value_C},
    case Value of
        Value_S ->
            %% We have a match.
            loop2([Item1 | List_Head], List_Tail, true);
        _ ->
            %% No match, continue.
            loop(Name, Value, undefined, List_Tail, [Item1 | List_Head], true)
    end;
loop(Name, Value, Type_Mod,
  [{Name, _, Type_Mod, Value} | _] = List_Tail,
  List_Head, Converted) when Type_Mod /= undefined ->
    %% Item's value is already compatible with Type_Mod and we have a match.
    loop2(List_Head, List_Tail, Converted);
loop(Name, Value, Type_Mod,
  [{Name, Value_S0, Other_Type_Mod, Value_C0} = Item | List_Tail],
  List_Head, Converted) when Type_Mod /= undefined ->
    %% Item's value must be converted to Type_Mod first.
    Value_S = case Value_S0 of
        undefined -> Other_Type_Mod:to_string(Value_C0);
        _         -> Value_S0
    end,
    try
        Value_C = Type_Mod:from_string(Value_S),
        Item1   = {Name, Value_S, Type_Mod, Value_C},
        case Value of
            Value_C ->
                %% We have a match.
                loop2([Item1 | List_Head], List_Tail, true);
            _ ->
                %% No match, continue.
                loop(Name, Value, Type_Mod, List_Tail, [Item1 | List_Head],
                  true)
        end
    catch
        throw:conversion_failed ->
            loop(Name, Value, Type_Mod, List_Tail,
              [Item | List_Head], Converted)
    end;
loop(Name, Value, Type_Mod,
  [Item | List_Tail], List_Head, Converted) ->
    loop(Name, Value, Type_Mod,
      List_Tail, [Item | List_Head], Converted);
loop(_, _, _, [], _, false) ->
    nomatch;
loop(_, _, _, [], List, true) ->
    List1 = lists:reverse(List),
    {nomatch, List1}.

loop2(_, _, false) ->
    match;
loop2(List_Head, List_Tail, true) ->
    List1 = lists:reverse(List_Head) ++ List_Tail,
    {match, List1}.
