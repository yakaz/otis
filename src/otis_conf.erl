%-
% Copyright (c) 2012-2014 Yakaz
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
% 1. Redistributions of source code must retain the above copyright
%    notice, this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright
%    notice, this list of conditions and the following disclaimer in the
%    documentation and/or other materials provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
% SUCH DAMAGE.

-module(otis_conf).

-include_lib("yamerl/include/yamerl_errors.hrl").
-include_lib("yamerl/include/yamerl_nodes.hrl").

-include("otis.hrl").

%% Public API.
-export([
    load/0,
    load/1,
    create_exprs/2,
    format_error/3,
    format_error/4
  ]).

-define(KEYWORD(K),  K#yamerl_str.text).

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
        YAML_Opts = [
          {detailed_constr, true},
          {node_mods, [
              yamerl_node_ipaddr
            ]}
        ],
        YAML = case yamerl_constr:file(File, YAML_Opts) of
            [#yamerl_doc{} = D | _] -> D;
            []                    -> #yamerl_doc{root = #yamerl_seq{}}
        end,
        Ruleset1 = Ruleset#ruleset{
          yaml = YAML
        },
        interpret_yamerl_doc(Ruleset1)
    catch
        throw:#yamerl_exception{errors = Errors} ->
            %% The YAML configuration file contains at least one error.
            %% Display what the parser returned.
            Fun = fun(
              #yamerl_parsing_error{text = Text} = E) ->
                Type1 = case E#yamerl_parsing_error.type of
                    error   -> "";
                    warning -> "Warning: "
                end,
                Line1 = case E#yamerl_parsing_error.line of
                    undefined -> "-";
                    Line      -> integer_to_list(Line)
                end,
                Col1 = case E#yamerl_parsing_error.column of
                    undefined -> "-";
                    Col       -> integer_to_list(Col)
                end,
                io_lib:format("~s:~s: ~s~s~n", [Line1, Col1, Type1, Text])
            end,
            Text = string:join(lists:map(Fun, Errors), ""),
            format_error(Ruleset, "~s", [Text])
    end.

interpret_yamerl_doc(
  #ruleset{yaml = #yamerl_doc{root = #yamerl_seq{entries = Items}}} = Ruleset) ->
    interpret_yamerl_items(Ruleset, Items, []);
interpret_yamerl_doc(
  #ruleset{yaml = #yamerl_doc{root = Node}} = Ruleset) ->
    %% This YAML document doesn't contain a sequence.
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: Expected a sequence of configuration blocks.~n",
      [Line, Col]).

interpret_yamerl_items(Ruleset, [#yamerl_map{pairs = Pairs} | Rest],
  Rules) ->
    case interpret_yamerl_item(Ruleset, Pairs) of
        {rule, Rule} ->
            interpret_yamerl_items(Ruleset, Rest, [Rule | Rules]);
        {hooks, Hooks} ->
            Ruleset1 = Ruleset#ruleset{
              hooks = Hooks
            },
            interpret_yamerl_items(Ruleset1, Rest, Rules);
        {consts, Consts} ->
            Ruleset1 = Ruleset#ruleset{
              consts = Consts
            },
            interpret_yamerl_items(Ruleset1, Rest, Rules)
    end;
interpret_yamerl_items(Ruleset, [], Rules) ->
    Ruleset#ruleset{
      rules = lists:reverse(Rules)
    };
interpret_yamerl_items(Ruleset, [Node | _], _) ->
    %% One sequence item isn't a map.
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: Expected a map of the form \"attribute: value\".~n",
      [Line, Col]).

interpret_yamerl_item(Ruleset, [{#yamerl_str{text = Keyword}, _} | _] = Pairs) when
  Keyword == "name" orelse
  Keyword == "desc" orelse
  Keyword == "rule" orelse
  Keyword == "hooks" orelse
  Keyword == "consts" ->
    create_rule(Ruleset, #rule{}, Pairs);
interpret_yamerl_item(Ruleset,
  [{#yamerl_str{text = "global hooks"} = Keyword, Args}]) ->
    create_hooks(Ruleset, Keyword, Args);
interpret_yamerl_item(Ruleset,
  [{#yamerl_str{text = "global hooks"} = Node, _} | _]) ->
    %% A "global hooks" attribute must be alone.
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: The \"global hooks\" attribute must be the only key "
      "of the map.~n",
      [Line, Col]);
interpret_yamerl_item(#ruleset{consts = Consts} = Ruleset,
  [{#yamerl_str{text = "global consts"} = Keyword, Args}]) ->
    create_consts(Ruleset, Consts, Keyword, Args);
interpret_yamerl_item(Ruleset,
  [{#yamerl_str{text = "global consts"} = Node, _} | _]) ->
    %% A "global consts" attribute must be alone.
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: The \"global consts\" attribute must be the only key "
      "of the map.~n",
      [Line, Col]);
interpret_yamerl_item(Ruleset, [{Node, _} | _]) ->
    %% This item is not recognized, display an error.
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: Unrecognized configuration block.~n",
      [Line, Col]).

create_rule(Ruleset, Rule, [{Attr, _} | _] = Pairs) ->
    Rule1 = Rule#rule{
      line = yamerl_constr:node_line(Attr),
      col  = yamerl_constr:node_column(Attr)
    },
    create_rule2(Ruleset, Rule1, Pairs).

create_rule2(Ruleset, Rule,
  [{#yamerl_str{text = "name"}, #yamerl_str{text = Name}} | Rest]) ->
    Rule1 = Rule#rule{
      name = Name
    },
    create_rule2(Ruleset, Rule1, Rest);
create_rule2(Ruleset, Rule,
  [{#yamerl_str{text = "desc"}, #yamerl_str{text = Desc}} | Rest]) ->
    Rule1 = Rule#rule{
      desc = Desc
    },
    create_rule2(Ruleset, Rule1, Rest);
create_rule2(Ruleset, Rule,
  [{#yamerl_str{text = "rule"}, #yamerl_seq{entries = Args}} | Rest]) ->
    Exprs = create_exprs(Ruleset, Args),
    Rule1 = Rule#rule{
      exprs = Exprs
    },
    create_rule2(Ruleset, Rule1, Rest);
create_rule2(Ruleset, Rule,
  [{#yamerl_str{text = "hooks"} = Keyword, Args} | Rest]) ->
    {hooks, Hooks} = create_hooks(Ruleset, Keyword, Args),
    Rule1 = Rule#rule{
      hooks = Hooks
    },
    create_rule2(Ruleset, Rule1, Rest);
create_rule2(Ruleset, #rule{consts = Consts} = Rule,
  [{#yamerl_str{text = "consts"} = Keyword, Args} | Rest]) ->
    {consts, Consts1} = create_consts(Ruleset, Consts, Keyword, Args),
    Rule1 = Rule#rule{
      consts = Consts1
    },
    create_rule2(Ruleset, Rule1, Rest);
create_rule2(_, Rule, []) ->
    {rule, Rule};
create_rule2(Ruleset, _, [{Attr, _} | _]) ->
    %% This rule attribute isn't recognized, display an error.
    Line = yamerl_constr:node_line(Attr),
    Col  = yamerl_constr:node_column(Attr),
    format_error(Ruleset,
      "~b:~b: Unrecognized or malformed rule attribute.~n",
      [Line, Col]).

create_hooks(Ruleset, Keyword, #yamerl_map{pairs = Pairs}) ->
    Hooks = #hooks{
      line = yamerl_constr:node_line(Keyword),
      col  = yamerl_constr:node_column(Keyword)
    },
    create_hooks2(Ruleset, Hooks, Pairs);
create_hooks(Ruleset, _, Node) ->
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: Expected a map of the form \"hook: expressions\".~n",
      [Line, Col]).

create_hooks2(Ruleset, Hooks,
  [{#yamerl_str{text = "rule init"}, #yamerl_seq{entries = Args}} | Rest]) ->
    Exprs = create_exprs(Ruleset, Args),
    Hooks1 = Hooks#hooks{
      rule_init = Exprs
    },
    create_hooks2(Ruleset, Hooks1, Rest);
create_hooks2(Ruleset, Hooks,
  [{#yamerl_str{text = "if continue"}, #yamerl_seq{entries = Args}} | Rest]) ->
    Exprs = create_exprs(Ruleset, Args),
    Hooks1 = Hooks#hooks{
      if_continue = Exprs
    },
    create_hooks2(Ruleset, Hooks1, Rest);
create_hooks2(Ruleset, Hooks,
  [{#yamerl_str{text = "if final"}, #yamerl_seq{entries = Args}} | Rest]) ->
    Exprs = create_exprs(Ruleset, Args),
    Hooks1 = Hooks#hooks{
      if_final = Exprs
    },
    create_hooks2(Ruleset, Hooks1, Rest);
create_hooks2(Ruleset, Hooks,
  [{#yamerl_str{text = "if abort"}, #yamerl_seq{entries = Args}} | Rest]) ->
    Exprs = create_exprs(Ruleset, Args),
    Hooks1 = Hooks#hooks{
      if_abort = Exprs
    },
    create_hooks2(Ruleset, Hooks1, Rest);
create_hooks2(_, Hooks, []) ->
    {hooks, Hooks};
create_hooks2(Ruleset, _, [{Attr, _} | _]) ->
    %% This hook isn't recognized, display an error.
    Line = yamerl_constr:node_line(Attr),
    Col  = yamerl_constr:node_column(Attr),
    format_error(Ruleset,
      "~b:~b: Unrecognized or malformed hook.~n",
      [Line, Col]).

create_consts(Ruleset, Consts, _, #yamerl_map{pairs = Pairs}) ->
    create_consts2(Ruleset, Consts, Pairs);
create_consts(Ruleset, _, _, Node) ->
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: Expected a map of the form \"const: value\".~n",
      [Line, Col]).

create_consts2(#ruleset{consts = Global_Consts} = Ruleset, Consts,
  [{#yamerl_str{text = Const} = Node, #yamerl_str{text = Value}}
  | Rest]) ->
    Const1 = otis_var:parse(Const),
    Const2 = otis_var:expand_consts(Consts, Global_Consts, Const1),
    case Const2 of
        #var{prefix = undefined, name = Name, attr = undefined} ->
            Value1 = otis_var:parse(Value),
            Value2 = otis_var:expand_consts(Consts, Global_Consts, Value1),
            Consts1  = dict:store(Name, Value2, Consts),
            create_consts2(Ruleset, Consts1, Rest);
        _ ->
            Line = yamerl_constr:node_line(Node),
            Col  = yamerl_constr:node_column(Node),
            format_error(Ruleset,
              "~b:~b: Left operand must be a variable.~n",
              [Line, Col])
    end;
create_consts2(_, Consts, []) ->
    {consts, Consts};
create_consts2(Ruleset, _, [{Node, _} |_]) ->
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    format_error(Ruleset,
      "~b:~b: Expected a map of the form \"const: value\".~n",
      [Line, Col]).

create_exprs(Ruleset, Ops) ->
    create_exprs2(Ruleset, Ops, []).

create_exprs2(Ruleset,
  [#yamerl_map{pairs =
      [{#yamerl_str{text = "not " ++ Name} = Keyword, Args}]} = Not | Rest],
  Ops) ->
    %% The user prefixed a keyword with "not". We transform this to:
    %%   not:
    %%     Name
    Args1 = #yamerl_seq{
      entries = [#yamerl_map{pairs = [{Keyword#yamerl_str{text = Name}, Args}]}],
      pres    = Not#yamerl_map.pres
    },
    New_Ops = otis_op_not:create(Ruleset, Keyword, Args1),
    create_exprs2(Ruleset, Rest, Ops ++ New_Ops);
create_exprs2(Ruleset,
  [#yamerl_map{pairs = [{#yamerl_str{text = Name} = Keyword, Args}]} = Node | Rest],
  Ops) ->
    Mod = list_to_atom("otis_op_" ++ Name),
    case code:ensure_loaded(Mod) of
        {module, _} ->
            New_Ops = Mod:create(Ruleset, Keyword, Args),
            create_exprs2(Ruleset, Rest, Ops ++ New_Ops);
        {error, Reason} ->
            Line = yamerl_constr:node_line(Node),
            Col  = yamerl_constr:node_column(Node),
            format_error(Ruleset,
              "~b:~b: Unsupported keyword '~s' (~s: ~p).~n",
              [Line, Col, Name, Mod, Reason])
    end;
create_exprs2(_, [], Ops) ->
    Ops;
create_exprs2(Ruleset, [Node | _], _) ->
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    format_error(Ruleset, "~b:~b: Expected a keyword (string).~n",
      [Line, Col]).

format_error(#ruleset{file = File}, Format, Args) ->
    ?ERROR(
      "~n"
      "Invalid configuration file '~s':~n" ++
      Format, [File | Args]),
    throw(invalid_conf).

format_error(#ruleset{file = File}, Keyword, Format, Args) ->
    Line = yamerl_constr:node_line(Keyword),
    Col  = yamerl_constr:node_column(Keyword),
    ?ERROR(
      "~n"
      "Invalid configuration file '~s':~n" ++
      "~b:~b: Invalid \"~s\" expression:~n" ++
      Format, [File, Line, Col, ?KEYWORD(Keyword) | Args]),
    throw(invalid_conf).
