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
    {Scheme0, Host0, Port0, Path, Query} = otis_utils:parse_uri(URI),
    %% Set default values for scheme/host/port if the given URL
    %% specified only the path, which would be valid in HTTP/1.0 (ie. no
    %% host in the URL and no "Host" header).
    Scheme = case Scheme0 of
        undefined -> "http";
        _         -> Scheme0
    end,
    Host = case Host0 of
        undefined ->
            case State#state.vhost_name of
                undefined -> throw(missing_host);
                _         -> State#state.vhost_name
            end;
        _ ->
            Host0
    end,
    Port = case Port0 of
        undefined -> 80;
        _         -> Port0
    end,
    Server_IP = otis_utils:server_ip(Host),
    Headers1  = [
      {string:to_lower(H), V, undefined, undefined}
      || {H, V} <- Headers
    ],
    Http_Ver = case Host0 of
        undefined -> {1, 0};
        _         -> {1, 1}
    end,
    State1 = State#state{
      server_ip   = Server_IP,
      server_port = Port,
      http_ver    = Http_Ver,
      method      = "GET",
      scheme      = Scheme,
      host        = Host,
      path        = Path,
      headers     = Headers1,
      query_str   = Query
    },
    State2 = case State1#state.vhost_name of
        undefined ->
            State1#state{
              vhost_name = Host
            };
        _ ->
            State1
    end,
    State3 = case otis_var:get_header(State2, "host") of
        {_, undefined} ->
            Host1 = otis_utils:format_host(Scheme, Host, Port),
            otis_var:set_header(State2, "host", Host1);
        _ ->
            State2
    end,
    eval(State3).

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
