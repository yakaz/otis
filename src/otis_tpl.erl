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

-module(otis_tpl).

-include("otis.hrl").
-include("otis_codegen.hrl").

%% Public API.
-export([
    read/1,
    gen/1,
    gen_exprs/3,
    expand/2,
    fun_name/1,
    get_rule_fun/2
  ]).

%% -------------------------------------------------------------------
%% Template handling.
%% -------------------------------------------------------------------

read(Tpl_Name) ->
    Tpl_Name1 = filename(Tpl_Name),
    case file:read_file(Tpl_Name1) of
        {ok, Content} ->
            Content;
        {error, Reason} ->
            ?ERROR("Failed to read template \"~s\": ~s~n",
              [Tpl_Name1, file:format_error(Reason)]),
            throw(invalid_template)
    end.

filename(Name) ->
    case otis_app:get_param(templates_dir) of
        default ->
            case otis_app:from_builddir() of
                false ->
                    Dir = code:priv_dir(?APPLICATION),
                    filename:join([Dir, "template", Name]);
                true ->
                    Src_Dir = os:getenv("srcdir"),
                    filename:join([Src_Dir, "..", "priv", "template", Name])
            end;
        Dir ->
            filename:join([Dir, Name])
    end.

expand(Tpl, [{Name, Value} | Rest]) ->
    Value1 = re:replace(Value, [$\\, $\\], [$\\, $\\, $\\, $\\],
      [global,  {return, list}]),
    Value2 = re:replace(Value1, [$&], [$\\, $\\, $&],
      [global,  {return, list}]),
    Tpl1 = re:replace(Tpl, "%%#.*\n\n?", "", [global, {return, list}]),
    Tpl2 = re:replace(Tpl1, [$\\, $? | Name], Value2, [global, {return, list}]),
    expand(Tpl2, Rest);
expand(Tpl, []) ->
    Tpl.

%% -------------------------------------------------------------------
%% Generated functions naming.
%% -------------------------------------------------------------------

fun_name(#code{cursor = Cursor}) ->
    fun_name(Cursor);
fun_name(Cursor) ->
    Cursor1 = [
      case Cmp of
          {C, I}               -> io_lib:format("~s~b", [C, I]);
          I when is_integer(I) -> integer_to_list(I);
          C                    -> io_lib:format("~s", [C])
      end
      || Cmp <- Cursor
    ],
    lists:flatten(string:join(lists:reverse(Cursor1), "_")).

get_rule_fun(Code, first) ->
    get_rule_fun(Code, {rule, 1});
get_rule_fun(#code{rules_count = Count} = Code, last) ->
    get_rule_fun(Code, {rule, Count});
get_rule_fun(#code{cursor = Cursor} = Code, previous) ->
    [{rule, I} | _] = lists:reverse(Cursor),
    Prev = if
        I == 1 -> 1;
        true   -> I - 1
    end,
    get_rule_fun(Code, {rule, Prev});
get_rule_fun(#code{cursor = Cursor, rules_count = Count} = Code, next) ->
    [{rule, I} | _] = lists:reverse(Cursor),
    Next = if
        I == Count -> I;
        true       -> I + 1
    end,
    get_rule_fun(Code, {rule, Next});
get_rule_fun(#code{rules_idx = Index}, Name) ->
    dict:fetch(Name, Index).

index_rules(Rules) ->
    index_rules2(Rules, [], 0, dict:new()).

index_rules2([#rule{name = undefined} = Rule | Rest], Named, Count, Index) ->
    Name     = lists:flatten(io_lib:format("Rule #~b", [Count + 1])),
    Count1   = Count + 1,
    Fun_Name = fun_name([{rule, Count1}]),
    Index1   = dict:store(Name, Fun_Name, Index),
    Index2   = dict:store({rule, Count1}, Fun_Name, Index1),
    index_rules2(Rest, [Rule#rule{name = Name} | Named], Count1, Index2);
index_rules2([#rule{name = Name} = Rule | Rest], Named, Count, Index) ->
    Count1   = Count + 1,
    Fun_Name = fun_name([{rule, Count1}]),
    Index1   = dict:store(Name, Fun_Name, Index),
    Index2   = dict:store({rule, Count1}, Fun_Name, Index1),
    index_rules2(Rest, [Rule | Named], Count1, Index2);
index_rules2([], Named, Count, Index) ->
    {Named, Index, Count}.

%% -------------------------------------------------------------------
%% Code generation.
%% -------------------------------------------------------------------

gen(#ruleset{rules = Rules} = Ruleset) ->
    %% Initialize the code generation state.
    {Rules1, Rules_Idx, Rules_Count} = index_rules(Rules),
    Code = #code{
      rules_idx   = Rules_Idx,
      rules_count = Rules_Count,
      ruleset     = Ruleset
    },
    Fun = fun({Key, Value}, Idx) ->
            Line = io_lib:format("      {~p, fun ~s/1}", [Key, Value]),
            [Line | Idx]
    end,
    Rules_Idx_Var = "[\n" ++
      string:join(
        lists:reverse(lists:foldl(Fun, [],
            lists:keysort(1, dict:to_list(Rules_Idx)))), ",\n") ++
    "\n    ]",
    %% Generate code for rules.
    {First_Rule, Rules_Text} = gen_rules(Code, Rules1, "otis_utils:return"),
    %% Generation timestamp.
    {{Y, Mo, D}, {H, Mi, S}} = erlang:localtime(),
    Timestamp = io_lib:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b",
      [Y, Mo, D, H, Mi, S]),
    %% Generate module header.
    Tpl      = read("otis_reqrw_engine.erl"),
    Tpl_Vars = [
      {"SRC_FILE",   Ruleset#ruleset.file},
      {"TIMESTAMP",  Timestamp},
      {"FIRST_RULE", First_Rule},
      {"RULES_IDX",  Rules_Idx_Var},
      {"CONF",       io_lib:format("~p", [Ruleset#ruleset.file])},
      {"RULES",      io_lib:format("~p", [Rules])}
    ],
    Text = expand(Tpl, Tpl_Vars),
    string:join([Text | Rules_Text], "\n").

gen_rules(#code{rules_count = Count} = Code, Rules, Next_Fun) ->
    Code1 = Code#code{
      cursor = [{rule, Count}]
    },
    gen_rules2(Code1, Rules, Next_Fun, []).

gen_rules2(#code{cursor = [{rule, I}], ruleset = Ruleset} = Code,
  [#rule{name = Name, desc = Desc, exprs = Exprs,
      line = Line, col = Col} = Rule | Rest],
  Next_Fun, Rules_Text) ->
    Code1 = Code#code{
      current = Rule
    },
    %% Get the rule function name and generate the expressions code.
    Rule_Fun   = get_rule_fun(Code1, Name),
    First_Rule = get_rule_fun(Code1, first),
    Prev_Rule  = get_rule_fun(Code1, previous),
    Last_Rule  = get_rule_fun(Code1, last),
    Return_Fun = "otis_utils:return",
    {First_Expr, Exprs_Text} = gen_exprs(Code1, Exprs, Return_Fun),
    Hook_Vars  = [
      {"FIRST_EXPR", First_Expr},
      {"FIRST_RULE", First_Rule},
      {"PREV_RULE",  Prev_Rule},
      {"NEXT_RULE",  Next_Fun},
      {"LAST_RULE",  Last_Rule}
    ],
    {[RInit_Fun], Before_Text} = gen_hooks(Code1, Rule, [rule_init], Hook_Vars,
      Return_Fun),
    {[Cont_Fun, Final_Fun, Abort_Fun], After_Text} = gen_hooks(Code1, Rule,
      [if_continue, if_final, if_abort], Hook_Vars, Return_Fun),
    %% Generate the rule code.
    Tpl      = read("rule.erl"),
    %% Prepare template variables.
    Name_S = io_lib:format("~p", [lists:flatten(Name)]),
    Desc1  = case Desc of
        undefined ->
            "%% (no description)";
        _ ->
            re:replace(Desc, "^", "%% ", [global, multiline, {return, list}])
    end,
    Tpl_Vars = [
      {"FUNCTION",         Rule_Fun},
      {"NAME_S",           Name_S},
      {"NAME",             Name},
      {"DESC",             Desc1},
      {"RULE_INIT_HOOK",   RInit_Fun},
      {"IF_CONTINUE_HOOK", Cont_Fun},
      {"IF_FINAL_HOOK",    Final_Fun},
      {"IF_ABORT_HOOK",    Abort_Fun},
      {"SRC_FILE",         Ruleset#ruleset.file},
      {"SRC_LINE",         io_lib:format("~b", [Line])},
      {"SRC_COL",          io_lib:format("~b", [Col])}
    ],
    Text = expand(Tpl, Tpl_Vars),
    Rule_Text = [Text | Before_Text] ++ Exprs_Text ++ After_Text,
    %% Decrement the current rule position.
    Code2 = Code#code{
      cursor = [{rule, I - 1}]
    },
    gen_rules2(Code2, Rest, Rule_Fun, Rule_Text ++ Rules_Text);
gen_rules2(_, [], First_Fun, Rules_Text) ->
    {First_Fun, Rules_Text}.

gen_hooks(#code{ruleset = #ruleset{hooks = GHooks}} = Code,
  #rule{hooks = Hooks}, Names, Vars, Next_Fun) ->
    gen_hooks2(Code, Names, Vars, GHooks, Hooks, Next_Fun, [], []).

gen_hooks2(#code{cursor = Cursor} = Code, [Name | Rest], Vars,
  GHooks, Hooks, Next_Fun, Funs, Texts) ->
    Cursor1 = [Name | Cursor],
    Code1 = Code#code{
      cursor = Cursor1
    },
    Hook_Fun = fun_name(Cursor1),
    Exprs = case Name of
        rule_init   -> GHooks#hooks.rule_init  ++ Hooks#hooks.rule_init;
        if_continue -> Hooks#hooks.if_continue ++ GHooks#hooks.if_continue;
        if_final    -> Hooks#hooks.if_final    ++ GHooks#hooks.if_final;
        if_abort    -> Hooks#hooks.if_abort    ++ GHooks#hooks.if_abort
    end,
    {Expr_Fun, Exprs_Text} = gen_exprs(Code1, Exprs, Next_Fun),
    Tpl_Name = case Exprs of
        [] -> "nohook_" ++ atom_to_list(Name) ++ ".erl";
        _  -> "hook_" ++ atom_to_list(Name) ++ ".erl"
    end,
    Tpl      = read(Tpl_Name),
    Src_File = (Code#code.ruleset)#ruleset.file,
    {Src_GLine, Src_GCol} = case GHooks#hooks.line of
        undefined -> {"-", "-"};
        GL        -> {integer_to_list(GL), integer_to_list(GHooks#hooks.col)}
    end,
    {Src_Line, Src_Col} = case Hooks#hooks.line of
        undefined -> {"-", "-"};
        L         -> {integer_to_list(L), integer_to_list(Hooks#hooks.col)}
    end,
    Tpl_Vars = [
      {"FUNCTION",  Hook_Fun},
      {"HOOK_EXPR", Expr_Fun},
      {"SRC_GFILE", Src_File},
      {"SRC_GLINE", Src_GLine},
      {"SRC_GCOL",  Src_GCol},
      {"SRC_FILE",  Src_File},
      {"SRC_LINE",  Src_Line},
      {"SRC_COL",   Src_Col}
      | Vars
    ],
    %% Expand template.
    Text = expand(Tpl, Tpl_Vars),
    gen_hooks2(Code, Rest, Vars, GHooks, Hooks, Next_Fun,
      [Hook_Fun | Funs], Texts ++ [Text | Exprs_Text]);
gen_hooks2(_, [], _, _, _, _, Funs, Texts) ->
    {lists:reverse(Funs), Texts}.

gen_exprs(#code{cursor = Cursor} = Code, Exprs, Next_Fun) ->
    %% We add a number to the cursor corresponding to the current
    %% expression position. Because we generate expressions backward,
    %% this number is initialized as the number of expressions.
    Exprs_Count = length(Exprs),
    Code1 = Code#code{
      cursor = [Exprs_Count + 1 | Cursor]
    },
    gen_exprs2(Code1, lists:reverse(Exprs), Next_Fun, []).

gen_exprs2(#code{cursor = [I | Cursor]} = Code, [Expr | Rest], Next_Fun,
  Exprs_Text) ->
    %% Decrement the current expression position; remember, expressions
    %% are handled backward.
    Code1 = Code#code{
      cursor = [I - 1 | Cursor]
    },
    %% Generate the code.
    Mod = ?EXPR_MOD(Expr),
    {Expr_Fun, Text} = Mod:gen_code(Code1, Expr, Next_Fun),
    %% Continue with the next expression.
    gen_exprs2(Code1, Rest, Expr_Fun, Text ++ Exprs_Text);
gen_exprs2(_, [], First_Fun, Exprs_Text) ->
    {First_Fun, Exprs_Text}.
