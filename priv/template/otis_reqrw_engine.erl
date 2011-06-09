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

-ifdef('IN_SRC').
-include("otis.hrl").
-else.
-include_lib("otis/include/otis.hrl").
-endif.

%% Public API.
-export([
    uri/1,
    uri/2,
    request/1,
    request/2,
    eval/1,
    call_rule/2,
    rules/0
  ]).

%% -------------------------------------------------------------------
%% Request rewriting API.
%% -------------------------------------------------------------------

uri(URI) ->
    uri(URI, []).

uri(URI, Headers) ->
    State = request2(URI, Headers),
    Var = otis_var:prepare_string("$(URI)"),
    {_, Result} = otis_var:expand(State, Var),
    lists:flatten(Result).

request(URI) ->
    request(URI, []).

request(URI, Headers) ->
    State = request2(URI, Headers),
    Var = otis_var:prepare_string("$(RESULT)"),
    {_, Result} = otis_var:expand(State, Var),
    lists:flatten(Result).

request2(URI, Headers) ->
    {Scheme, Host, Port, Path, Query} = otis_utils:parse_uri(URI),
    Server_IP = otis_utils:server_ip(Host),
    Headers1  = [
      {string:to_lower(H), V, undefined, undefined}
      || {H, V} <- Headers
    ],
    State = #state{
      server_name = Host,
      server_ip   = Server_IP,
      server_port = Port,
      method      = "GET",
      scheme      = Scheme,
      path        = Path,
      headers     = Headers1,
      query_str   = Query
    },
    State1 = case otis_var:get_header(State, "host") of
        {_, undefined} ->
            Host1 = otis_utils:format_host(Scheme, Host, Port),
            otis_var:set_header(State, "host", Host1);
        _ ->
            State
    end,
    eval(State1).

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
