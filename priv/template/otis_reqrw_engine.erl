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
    uri/3,
    request/1,
    request/2,
    request/3,
    eval/1,
    call_rule/2,
    conf/0,
    rules/0
  ]).

%% -------------------------------------------------------------------
%% Request rewriting API.
%% -------------------------------------------------------------------

uri(URI) ->
    uri(URI, []).

uri(URI, Headers) ->
    uri(URI, Headers, #state{}).

uri(URI, Headers, State) ->
    State1 = request2(URI, Headers, State),
    Var = otis_var:parse("$(URI)"),
    {_, Result} = otis_var:expand(State1, Var),
    lists:flatten(Result).

request(URI) ->
    request(URI, []).

request(URI, Headers) ->
    request(URI, Headers, #state{}).

request(URI, Headers, State) ->
    State1 = request2(URI, Headers, State),
    Var = otis_var:parse("$(RESULT)"),
    {_, Result} = otis_var:expand(State1, Var),
    lists:flatten(Result).

request2(URI, Headers, State) ->
    {Scheme, Host, Port, Path, Query} = otis_utils:parse_uri(URI),
    Server_IP = otis_utils:server_ip(Host),
    Headers1  = [
      {string:to_lower(H), V, undefined, undefined}
      || {H, V} <- Headers
    ],
    State1 = State#state{
      server_name = Host,
      server_ip   = Server_IP,
      server_port = Port,
      method      = "GET",
      scheme      = Scheme,
      path        = Path,
      headers     = Headers1,
      query_str   = Query
    },
    State2 = case otis_var:get_header(State1, "host") of
        {_, undefined} ->
            Host1 = otis_utils:format_host(Scheme, Host, Port),
            otis_var:set_header(State1, "host", Host1);
        _ ->
            State1
    end,
    eval(State2).

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

conf() ->
    ?CONF.

rules() ->
    ?RULES.
