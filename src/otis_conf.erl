-module(otis_conf).

-include_lib("yaml/include/yaml_parser.hrl").
-include_lib("yaml/include/yaml_nodes.hrl").

-include("otis.hrl").

%% Public API.
-export([
    load/0,
    load/1,
    create_exprs/2,
    format_error/3,
    format_error/4
  ]).

-define(KEYWORD(K),  K#yaml_str.text).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

load() ->
    case otis_app:get_param(config) of
        none -> undefined;
        File -> load(File)
    end.

load(File) ->
    Ruleset = #ruleset{
      file = File
    },
    try
        YAML = case yaml_repr:file(File, [{simple_structs, false}]) of
            [#yaml_doc{} = D | _] -> D;
            []                    -> #yaml_doc{root = #yaml_seq{}}
        end,
        Ruleset1 = Ruleset#ruleset{
          yaml = YAML
        },
        interpret_yaml_doc(Ruleset1)
    catch
        throw:{yaml_parser, Parser} ->
            %% The YAML configuration file contains at least one error.
            %% Display what the parser returned.
            Errors = yaml_parser:get_errors(Parser),
            Fun = fun(
              #yaml_parser_error{line = Line, column = Col, text = Text}) ->
                io_lib:format("~b:~b: ~s~n", [Line, Col, Text])
            end,
            Text = string:join(lists:map(Fun, Errors), ""),
            format_error(Ruleset, "~s", [Text])
    end.

interpret_yaml_doc(
  #ruleset{yaml = #yaml_doc{root = #yaml_seq{entries = Items}}} = Ruleset) ->
    interpret_yaml_items(Ruleset, Items, []);
interpret_yaml_doc(
  #ruleset{yaml = #yaml_doc{root = Node}} = Ruleset) ->
    %% This YAML document doesn't contain a sequence.
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: Expected a sequence of configuration blocks.~n",
      [Line, Col]).

interpret_yaml_items(Ruleset, [#yaml_map{pairs = Pairs} | Rest],
  Rules) ->
    case interpret_yaml_item(Ruleset, Pairs) of
        {rule, Rule} ->
            interpret_yaml_items(Ruleset, Rest, [Rule | Rules]);
        {hooks, Hooks} ->
            Ruleset1 = Ruleset#ruleset{
              hooks = Hooks
            },
            interpret_yaml_items(Ruleset1, Rest, Rules);
        {consts, Consts} ->
            Ruleset1 = Ruleset#ruleset{
              consts = Consts
            },
            interpret_yaml_items(Ruleset1, Rest, Rules)
    end;
interpret_yaml_items(Ruleset, [], Rules) ->
    Ruleset#ruleset{
      rules = lists:reverse(Rules)
    };
interpret_yaml_items(Ruleset, [Node | _], _) ->
    %% One sequence item isn't a map.
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: Expected a map of the form \"attribute: value\".~n",
      [Line, Col]).

interpret_yaml_item(Ruleset, [{#yaml_str{text = Keyword}, _} | _] = Pairs) when
  Keyword == "name" orelse
  Keyword == "desc" orelse
  Keyword == "rule" orelse
  Keyword == "hooks" orelse
  Keyword == "consts" ->
    create_rule(Ruleset, #rule{}, Pairs);
interpret_yaml_item(Ruleset,
  [{#yaml_str{text = "global hooks"} = Keyword, Args}]) ->
    create_hooks(Ruleset, Keyword, Args);
interpret_yaml_item(Ruleset,
  [{#yaml_str{text = "global hooks"} = Node, _} | _]) ->
    %% A "global hooks" attribute must be alone.
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: The \"global hooks\" attribute must be the only key "
      "of the map.~n",
      [Line, Col]);
interpret_yaml_item(#ruleset{consts = Consts} = Ruleset,
  [{#yaml_str{text = "global consts"} = Keyword, Args}]) ->
    create_consts(Ruleset, Consts, Keyword, Args);
interpret_yaml_item(Ruleset,
  [{#yaml_str{text = "global consts"} = Node, _} | _]) ->
    %% A "global consts" attribute must be alone.
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: The \"global consts\" attribute must be the only key "
      "of the map.~n",
      [Line, Col]);
interpret_yaml_item(Ruleset, [{Node, _} | _]) ->
    %% This item is not recognized, display an error.
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: Unrecognized configuration block.~n",
      [Line, Col]).

create_rule(Ruleset, Rule, [{Attr, _} | _] = Pairs) ->
    Rule1 = Rule#rule{
      line = yaml_repr:node_line(Attr),
      col  = yaml_repr:node_column(Attr)
    },
    create_rule2(Ruleset, Rule1, Pairs).

create_rule2(Ruleset, Rule,
  [{#yaml_str{text = "name"}, #yaml_str{text = Name}} | Rest]) ->
    Rule1 = Rule#rule{
      name = Name
    },
    create_rule2(Ruleset, Rule1, Rest);
create_rule2(Ruleset, Rule,
  [{#yaml_str{text = "desc"}, #yaml_str{text = Desc}} | Rest]) ->
    Rule1 = Rule#rule{
      desc = Desc
    },
    create_rule2(Ruleset, Rule1, Rest);
create_rule2(Ruleset, Rule,
  [{#yaml_str{text = "rule"}, #yaml_seq{entries = Args}} | Rest]) ->
    Exprs = create_exprs(Ruleset, Args),
    Rule1 = Rule#rule{
      exprs = Exprs
    },
    create_rule2(Ruleset, Rule1, Rest);
create_rule2(Ruleset, Rule,
  [{#yaml_str{text = "hooks"} = Keyword, Args} | Rest]) ->
    {hooks, Hooks} = create_hooks(Ruleset, Keyword, Args),
    Rule1 = Rule#rule{
      hooks = Hooks
    },
    create_rule2(Ruleset, Rule1, Rest);
create_rule2(Ruleset, #rule{consts = Consts} = Rule,
  [{#yaml_str{text = "consts"} = Keyword, Args} | Rest]) ->
    {consts, Consts1} = create_consts(Ruleset, Consts, Keyword, Args),
    Rule1 = Rule#rule{
      consts = Consts1
    },
    create_rule2(Ruleset, Rule1, Rest);
create_rule2(_, Rule, []) ->
    {rule, Rule};
create_rule2(Ruleset, _, [{Attr, _} | _]) ->
    %% This rule attribute isn't recognized, display an error.
    Line = yaml_repr:node_line(Attr),
    Col  = yaml_repr:node_column(Attr),
    format_error(Ruleset,
      "~b:~b: Unrecognized or malformed rule attribute.~n",
      [Line, Col]).

create_hooks(Ruleset, Keyword, #yaml_map{pairs = Pairs}) ->
    Hooks = #hooks{
      line = yaml_repr:node_line(Keyword),
      col  = yaml_repr:node_column(Keyword)
    },
    create_hooks2(Ruleset, Hooks, Pairs);
create_hooks(Ruleset, _, Node) ->
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: Expected a map of the form \"hook: expressions\".~n",
      [Line, Col]).

create_hooks2(Ruleset, Hooks,
  [{#yaml_str{text = "rule init"}, #yaml_seq{entries = Args}} | Rest]) ->
    Exprs = create_exprs(Ruleset, Args),
    Hooks1 = Hooks#hooks{
      rule_init = Exprs
    },
    create_hooks2(Ruleset, Hooks1, Rest);
create_hooks2(Ruleset, Hooks,
  [{#yaml_str{text = "if continue"}, #yaml_seq{entries = Args}} | Rest]) ->
    Exprs = create_exprs(Ruleset, Args),
    Hooks1 = Hooks#hooks{
      if_continue = Exprs
    },
    create_hooks2(Ruleset, Hooks1, Rest);
create_hooks2(Ruleset, Hooks,
  [{#yaml_str{text = "if final"}, #yaml_seq{entries = Args}} | Rest]) ->
    Exprs = create_exprs(Ruleset, Args),
    Hooks1 = Hooks#hooks{
      if_final = Exprs
    },
    create_hooks2(Ruleset, Hooks1, Rest);
create_hooks2(Ruleset, Hooks,
  [{#yaml_str{text = "if abort"}, #yaml_seq{entries = Args}} | Rest]) ->
    Exprs = create_exprs(Ruleset, Args),
    Hooks1 = Hooks#hooks{
      if_abort = Exprs
    },
    create_hooks2(Ruleset, Hooks1, Rest);
create_hooks2(_, Hooks, []) ->
    {hooks, Hooks};
create_hooks2(Ruleset, _, [{Attr, _} | _]) ->
    %% This hook isn't recognized, display an error.
    Line = yaml_repr:node_line(Attr),
    Col  = yaml_repr:node_column(Attr),
    format_error(Ruleset,
      "~b:~b: Unrecognized or malformed hook.~n",
      [Line, Col]).

create_consts(Ruleset, Consts, _, #yaml_map{pairs = Pairs}) ->
    create_consts2(Ruleset, Consts, Pairs);
create_consts(Ruleset, _, _, Node) ->
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: Expected a map of the form \"const: value\".~n",
      [Line, Col]).

create_consts2(#ruleset{consts = Global_Consts} = Ruleset, Consts,
  [{#yaml_str{text = Const} = Node, #yaml_str{text = Value}}
  | Rest]) ->
    Const1 = otis_var:prepare_string(Const),
    Const2 = otis_var:expand_consts(Consts, Global_Consts, Const1),
    case Const2 of
        [#var{prefix = undefined, name = Name, attr = undefined}] ->
            Value1 = otis_var:prepare_string(Value),
            Value2 = otis_var:expand_consts(Consts, Global_Consts, Value1),
            Consts1  = dict:store(Name, Value2, Consts),
            create_consts2(Ruleset, Consts1, Rest);
        _ ->
            Line = yaml_repr:node_line(Node),
            Col  = yaml_repr:node_column(Node),
            format_error(Ruleset,
              "~b:~b: Left operand must be a variable.~n",
              [Line, Col])
    end;
create_consts2(_, Consts, []) ->
    {consts, Consts};
create_consts2(Ruleset, _, [{Node, _} |_]) ->
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: Expected a map of the form \"const: value\".~n",
      [Line, Col]).

create_exprs(Ruleset, Ops) ->
    create_exprs2(Ruleset, Ops, []).

create_exprs2(Ruleset,
  [#yaml_map{pairs =
      [{#yaml_str{text = "not " ++ Name} = Keyword, Args}]} = Not | Rest],
  Ops) ->
    %% The user prefixed a keyword with "not". We transform this to:
    %%   not:
    %%     Name
    Args1 = #yaml_seq{
      entries = [#yaml_map{pairs = [{Keyword#yaml_str{text = Name}, Args}]}],
      pres    = Not#yaml_map.pres
    },
    New_Ops = otis_op_not:create(Ruleset, Keyword, Args1),
    create_exprs2(Ruleset, Rest, Ops ++ New_Ops);
create_exprs2(Ruleset,
  [#yaml_map{pairs = [{#yaml_str{text = Name} = Keyword, Args}]} = Node | Rest],
  Ops) ->
    Mod = list_to_atom("otis_op_" ++ Name),
    case code:ensure_loaded(Mod) of
        {module, _} ->
            New_Ops = Mod:create(Ruleset, Keyword, Args),
            create_exprs2(Ruleset, Rest, Ops ++ New_Ops);
        {error, Reason} ->
            Line = yaml_repr:node_line(Node),
            Col  = yaml_repr:node_column(Node),
            format_error(Ruleset,
              "~b:~b: Unsupported keyword '~s' (~s: ~p).~n",
              [Line, Col, Name, Mod, Reason])
    end;
create_exprs2(_, [], Ops) ->
    Ops;
create_exprs2(Ruleset, [Node | _], _) ->
    Line = yaml_repr:node_line(Node),
    Col  = yaml_repr:node_column(Node),
    format_error(Ruleset, "~b:~b: Expected a keyword (string).~n",
      [Line, Col]).

format_error(#ruleset{file = File}, Format, Args) ->
    ?ERROR(
      "~n"
      "Invalid configuration file '~s':~n" ++
      Format, [File | Args]),
    throw(invalid_conf).

format_error(#ruleset{file = File}, Keyword, Format, Args) ->
    Line = yaml_repr:node_line(Keyword),
    Col  = yaml_repr:node_column(Keyword),
    ?ERROR(
      "~n"
      "Invalid configuration file '~s':~n" ++
      "~b:~b: Invalid \"~s\" expression:~n" ++
      Format, [File, Line, Col, ?KEYWORD(Keyword) | Args]),
    throw(invalid_conf).
