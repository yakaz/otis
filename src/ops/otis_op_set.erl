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

-module(otis_op_set).

-include_lib("yamerl/include/yamerl_nodes.hrl").
-include_lib("yamerl/include/yamerl_nodes_yamerl_extensions.hrl").

-include("otis.hrl").
-include("otis_codegen.hrl").

%% Public API.
-export([
    create/3,
    gen_code/3
  ]).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

create(Ruleset, Keyword, #yamerl_map{pairs = Args}) ->
    create2(Ruleset, Keyword, Args, []);
create(Ruleset, Keyword, Node) ->
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Expected a map of the form \"variable: value\".~n",
      [Line, Col]),
    throw(invalid_config).

create2(Ruleset, Keyword,
  [{#yamerl_str{text = Var} = Node, Value} | Rest], Vars) ->
    case otis_var:parse(Var) of
        #var{} = Var1 ->
            Vars1  = lists:keystore(Var1, 1, Vars, {Var1, Value}),
            create2(Ruleset, Keyword, Rest, Vars1);
        _ ->
            Line = yamerl_constr:node_line(Node),
            Col  = yamerl_constr:node_column(Node),
            otis_conf:format_error(Ruleset, Keyword,
              "~b:~b: Left operand must be a variable.~n",
              [Line, Col])
    end;
create2(Ruleset, Keyword, [], Vars) ->
    create3(Ruleset, Keyword, Vars, []);
create2(Ruleset, Keyword, [Node | _], _) ->
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Left operand must be a variable.~n",
      [Line, Col]).

create3(Ruleset, Keyword, [{Var, #yamerl_null{}} | Rest], Result) ->
    Op = create4(Ruleset, Keyword, Var, "", string),
    create3(Ruleset, Keyword, Rest, [Op | Result]);
create3(Ruleset, Keyword, [{Var, #yamerl_str{text = Value}} | Rest], Result) ->
    Value1 = otis_var:parse(Value),
    Op = create4(Ruleset, Keyword, Var, Value1, string),
    create3(Ruleset, Keyword, Rest, [Op | Result]);
create3(Ruleset, Keyword, [{Var, #yamerl_int{value = Value}} | Rest], Result) ->
    Op = create4(Ruleset, Keyword, Var, Value, int),
    create3(Ruleset, Keyword, Rest, [Op | Result]);
create3(Ruleset, Keyword, [{Var, #yamerl_ip_addr{address = Value}} | Rest],
  Result) ->
    Op = create4(Ruleset, Keyword, Var, Value, ipaddr),
    create3(Ruleset, Keyword, Rest, [Op | Result]);
create3(_, _, [], Result) ->
    lists:reverse(Result);
create3(Ruleset, Keyword, [{_, Node} | _], _) ->
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Type of right operand is unsupported.~n",
      [Line, Col]).

create4(_, Keyword, Var, Value, Type) when ?IS_VAR_WRITABLE(Var) ->
    Type1 = case otis_var:expected_type(Var) of
        undefined -> Type;
        T         -> T
    end,
    #op_set{
      var   = Var,
      value = Value,
      type  = Type1,
      line  = yamerl_constr:node_line(Keyword),
      col   = yamerl_constr:node_column(Keyword)
    };
create4(Ruleset, Keyword, #var{name = Name}, _, _) ->
    otis_conf:format_error(Ruleset, Keyword,
      "-:-: Trying to set the read-only variable \"~s\".~n",
      [Name]).

%% -------------------------------------------------------------------
%% Code generation
%% -------------------------------------------------------------------

gen_code(#code{cursor = Cursor, ruleset = Ruleset} = Code,
  #op_set{line = Line, col = Col} = Expr, Next_Fun) ->
    %% Generate the expression code.
    Tpl = try
        otis_tpl:read(get_tpl_name(Expr))
    catch
        throw:invalid_template ->
            ?ERROR("Unsupported expression: ~p~n", [Expr]),
            throw(invalid_expression)
    end,
    %% Prepare template variables.
    Expr_Fun = otis_tpl:fun_name([set | Cursor]),
    Tpl_Vars = get_tpl_vars(Code, Expr, [
        {"FUNCTION",  Expr_Fun},
        {"NEXT_EXPR", Next_Fun},
        {"SRC_FILE",  Ruleset#ruleset.file},
        {"SRC_LINE",  io_lib:format("~b", [Line])},
        {"SRC_COL",   io_lib:format("~b", [Col])}
      ]),
    Text = otis_tpl:expand(Tpl, Tpl_Vars),
    {Expr_Fun, [Text]}.

get_tpl_name(#op_set{var = #var{prefix = Prefix} = Var, type = Type}) ->
    case otis_var:domain(Var) of
        Prefix when Prefix /= "user" ->
            "set_" ++ Prefix ++ ".erl";
        Domain ->
            string:join(["set", Domain, atom_to_list(Type)], "_") ++ ".erl"
    end.

get_tpl_vars(Code,
  #op_set{var = #var{prefix = undefined} = Var, value = Value},
  Vars) ->
    Var1   = otis_var:expand_consts(Code, Var),
    Value1 = otis_var:expand_consts(Code, Value),
    Member = otis_var:state_member(Var1),
    [
      {"MEMBER", Member},
      {"VAR",    io_lib:format("~p", [Var1])},
      {"VALUE",  io_lib:format("~p", [Value1])}
      | Vars
    ];
get_tpl_vars(Code,
  #op_set{var = #var{name = Name}, value = Value, type = Type}, Vars) ->
    Name1    = otis_var:expand_consts(Code, Name),
    Value1   = otis_var:expand_consts(Code, Value),
    Type_Mod = otis_utils:type_mod(Type),
    [
      {"NAME",     io_lib:format("~p", [Name1])},
      {"VALUE",    io_lib:format("~p", [Value1])},
      {"TYPE_MOD", io_lib:format("~s", [Type_Mod])}
      | Vars
    ].
