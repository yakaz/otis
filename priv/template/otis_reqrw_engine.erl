%% -------------------------------------------------------------------
%% Request rewrite module.
%%
%% Generated from:
%%   ?SRC_FILE
%%
%% Generated at:
%%   ?TIMESTAMP
%% -------------------------------------------------------------------

-module(otis_reqrw_engine).

-include_lib("otis/include/otis.hrl").

%% Public API.
-export([
    request/1,
    eval/1,
    call_rule/2,
    rules/0
  ]).

%% -------------------------------------------------------------------
%% Request rewriting API.
%% -------------------------------------------------------------------

request(URI) ->
    {Scheme, Host, Port, Path, Query} = otis_utils:parse_uri(URI),
    Server_IP = otis_utils:server_ip(Host),
    Host1     = otis_utils:format_host(Scheme, Host, Port),
    State = #state{
      server_name = Host,
      server_ip   = Server_IP,
      server_port = Port,
      method      = "GET",
      scheme      = Scheme,
      path        = Path,
      headers     = [{"host", Host, undefined, undefined}],
      query_str   = Query
    },
    State1 = otis_var:set_header(State, "host", Host1),
    State2 = eval(State1),
    Var = otis_var:prepare_string("$(REQUEST)"),
    {_, Result} = otis_var:expand(State2, Var),
    io:format("~s~n", [Result]).

eval(State) ->
    ?FIRST_RULE(State).

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

call_rule(State, I) when is_integer(I) ->
    call_rule(State, {rule, I});
call_rule(State, Name) ->
    Rules_Idx = ?RULES_IDX,
    case proplists:get_value(Name, Rules_Idx) of
        undefined -> otis_utils:abort(State);
        Fun       -> Fun(State)
    end.

%% -------------------------------------------------------------------
%% Rules list.
%% -------------------------------------------------------------------

rules() ->
    ?RULES.
