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

-module(otis_op_not).

-include_lib("yamerl/include/yamerl_nodes.hrl").

-include("otis.hrl").
-include("otis_codegen.hrl").

%% Public API.
-export([
    create/3,
    gen_code/3
  ]).

%% -------------------------------------------------------------------
%% String parsing.
%% -------------------------------------------------------------------

create(Ruleset, Keyword, #yamerl_seq{entries = Sub_Exprs}) ->
    [#op_not{
        subexprs = otis_conf:create_exprs(Ruleset, Sub_Exprs),
        line     = yamerl_constr:node_line(Keyword),
        col      = yamerl_constr:node_column(Keyword)
      }];
create(Ruleset, Keyword, Node) ->
    Line = yamerl_constr:node_line(Node),
    Col  = yamerl_constr:node_column(Node),
    otis_conf:format_error(Ruleset, Keyword,
      "~b:~b: Expected a sequence of sub-expressions.~n",
      [Line, Col]).

%% -------------------------------------------------------------------
%% Code generation
%% -------------------------------------------------------------------

gen_code(#code{cursor = Cursor, ruleset = Ruleset} = Code,
  #op_not{subexprs = Sub_Exprs, line = Line, col = Col} = Expr, Next_Fun) ->
    Tpl = try
        otis_tpl:read("not.erl")
    catch
        throw:invalid_template ->
            ?ERROR("Unsupported expression: ~p~n", [Expr]),
            throw(invalid_expression)
    end,
    %% Generate subexpressions.
    Cursor1   = ['not' | Cursor],
    Expr_Fun  = otis_tpl:fun_name(Cursor1),
    Expr_Fun2 = Expr_Fun ++ "2",
    Code1     = Code#code{
      cursor = Cursor1
    },
    {Sub_Expr_Fun, Sub_Exprs_Text} = otis_tpl:gen_exprs(Code1, Sub_Exprs,
      Expr_Fun2),
    %% Prepare template variables.
    Tpl_Vars = [
      {"FUNCTION2", Expr_Fun2},
      {"FUNCTION",  Expr_Fun},
      {"SUB_EXPR",  Sub_Expr_Fun},
      {"NEXT_EXPR", Next_Fun},
      {"SRC_FILE",  Ruleset#ruleset.file},
      {"SRC_LINE",  io_lib:format("~b", [Line])},
      {"SRC_COL",   io_lib:format("~b", [Col])}
    ],
    Text = otis_tpl:expand(Tpl, Tpl_Vars),
    {Expr_Fun, [Text | Sub_Exprs_Text]}.
