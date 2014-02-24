-module(otis_var).

-include("otis.hrl").
-include("otis_codegen.hrl").

%% Public API.
-export([
    parse/1,
    domain/1,
    state_member/1,
    expected_type/1,
    is_var/1,
    expand_consts/2,
    expand_consts/3,
    expand/2,
    get/2,
    get/3,
    get_uservar/2,
    get_uservar/3,
    get_query/2,
    get_query/3,
    get_header/2,
    get_header/3,
    get_rheader/2,
    get_rheader/3,
    get_cookie/2,
    get_cookie/3,
    headers_to_cookies/1,
    cookies_to_headers/1,
    set/3,
    set/4,
    set_uservar/3,
    set_uservar/4,
    set_query/3,
    set_query/4,
    set_header/3,
    set_header/4,
    set_rheader/3,
    set_rheader/4,
    set_cookie/3,
    set_cookie/4,
    run_filters/3,
    run_filters/4
  ]).

-record(parser, {
    result  = [],
    current = undefined
  }).

-record(var_st, {
    name   = [],
    opened = true
  }).

%% -------------------------------------------------------------------
%% String parsing.
%% -------------------------------------------------------------------

parse(Not_String) when not is_list(Not_String) ->
    Not_String;
parse(String) ->
    find_variables(String, #parser{}).

%% Parse variable.
find_variables([$$, $( | Rest], Parser) ->
    %% Start of a variable name. It may be inside another variable:
    %%   ${header:X-${site_${vendor}}-Site}
    Parser1 = collapse(Parser),
    Parser2 = Parser1#parser{
      result = [#var_st{} | Parser1#parser.result]
    },
    find_variables(Rest, Parser2);

find_variables([$) | Rest],
  #parser{result = [#var_st{opened = true} | _]} = Parser) ->
    %% End of a variable.
    Parser1 = collapse(Parser),
    [Var1 | Result1] = Parser1#parser.result,
    Parser2 = Parser1#parser{
      result  = Result1,
      current = Var1
    },
    Parser3 = collapse(Parser2),
    find_variables(Rest, Parser3);

%% Parse text.
find_variables([$$, $$ | Rest], #parser{current = Text} = Parser) ->
    Text1 = case Text of
        undefined -> [$$];
        _         -> [$$ | Text]
    end,
    Parser1 = Parser#parser{
      current = Text1
    },
    find_variables(Rest, Parser1);
find_variables([C | Rest], #parser{current = Text} = Parser) ->
    Text1 = case Text of
        undefined -> [C];
        _         -> [C | Text]
    end,
    Parser1 = Parser#parser{
      current = Text1
    },
    find_variables(Rest, Parser1);

%% End of string.
find_variables([], #parser{result = [#var_st{} | _]}) ->
    ?ERROR("Missing ')' to finish variable name~n", []),
    throw(invalid_variable_name);
find_variables([], #parser{result = Result, current = undefined}) ->
    Result1 = lists:reverse(Result),
    case Result1 of
        [T] when is_list(T)        -> T;
        [V] when is_record(V, var) -> V;
        _                          -> Result1
    end;
find_variables([], Parser) ->
    %% Collapse one level.
    Parser1 = collapse(Parser),
    find_variables([], Parser1).

%% Reverse text or create final variable.
collapse(#parser{current = undefined} = Parser) ->
    Parser;
collapse(#parser{current = #var_st{name = []}}) ->
    ?ERROR("Invalid empty variable name~n", []),
    throw(invalid_variable_name);
collapse(#parser{current = #var_st{name = Name}} = Parser) ->
    %% Extract filters. They're placed at the end of a variable but Name
    %% is reversed.
    [Tail1 | Rest1]  = Name,
    {Tail2, Filters} = extract_filters(Tail1),
    %% Extract attribute. We use the same method than for filters. But
    %% first, we must check if Tail2 is empty. If this is the case, we
    %% need to pick the next element in Rest.
    [Tail3 | Rest2] = case Tail2 of
        "" when Rest1 /= [] ->
            Rest1;
        "" ->
            ?ERROR("Invalid empty variable name~n", []),
            throw(invalid_variable_name);
        _  ->
            [Tail2 | Rest1]
    end,
    {Tail4, Attr} = extract_attr(Tail3),
    %% Extract prefix. Like above, we must check if Tail4 is empty. The
    %% prefix is placed at the beginning: we need to reverse the list.
    [Head1 | Rest3] = case Tail4 of
        "" when Rest2 /= [] ->
            lists:reverse(Rest2);
        "" ->
            ?ERROR("Invalid empty variable name~n", []),
            throw(invalid_variable_name);
        _  ->
            lists:reverse([Tail4 | Rest2])
    end,
    {Head2, Prefix} = extract_prefix(Head1),
    %% And again, we check if the returned Head2 is empty.
    Name1 = case Head2 of
        "" when Rest3 /= [] ->
            Rest3;
        "" ->
            ?ERROR("Invalid empty variable name~n", []),
            throw(invalid_variable_name);
        _  ->
            [Head2 | Rest3]
    end,
    %% Unfold the list, if any.
    Name2 = case Name1 of
        [N] when is_list(N) -> N;
        _                   -> Name1
    end,
    if
        Name2 == "" ->
            ?ERROR("Invalid empty variable name~n", []),
            throw(invalid_variable_name);
        true ->
            ok
    end,
    Var = #var{
      prefix  = Prefix,
      name    = Name2,
      attr    = Attr,
      filters = Filters
    },
    collapse2(Parser, Var);
collapse(#parser{current = Text} = Parser) ->
    collapse2(Parser, lists:reverse(Text)).

extract_prefix(Name) when is_list(Name) ->
    case string:chr(Name, $:) of
        0 ->
            {Name, undefined};
        I ->
            case string:substr(Name, 1, I - 1) of
                "" ->
                    ?ERROR("Invalid empty variable prefix~n", []),
                    throw(invalid_variable_prefix);
                Prefix ->
                    {string:substr(Name, I + 1), Prefix}
            end
    end;
extract_prefix(Name) ->
    {Name, undefined}.

extract_attr(Name) when is_list(Name) ->
    case string:rchr(Name, $#) of
        0 ->
            {Name, undefined};
        I ->
            case string:substr(Name, I + 1) of
                "" ->
                    ?ERROR("Invalid empty variable attribute~n", []),
                    throw(invalid_variable_attr);
                Attr ->
                    {string:substr(Name, 1, I - 1), Attr}
            end
    end;
extract_attr(Name) ->
    {Name, undefined}.

extract_filters(Name) when is_list(Name) ->
    extract_filters(Name, []);
extract_filters(Name) ->
    {Name, []}.

extract_filters(Name, Filters) ->
    case string:rchr(Name, $|) of
        0 ->
            {Name, Filters};
        I ->
            case string:substr(Name, I + 1) of
                "" ->
                    ?ERROR("Invalid empty variable filter~n", []),
                    throw(invalid_variable_filter);
                Filter ->
                    extract_filters(string:substr(Name, 1, I - 1),
                      [Filter | Filters])
            end
    end.

%% Append it to the opened variable or the main list.
collapse2(#parser{result = [#var_st{opened = true} = Var | Result]} = Parser,
  Sub) ->
    Var1 = Var#var_st{name = [Sub | Var#var_st.name]},
    Parser#parser{result = [Var1 | Result], current = undefined};
collapse2(#parser{result = Result} = Parser, Sub) ->
    Parser#parser{
      result  = [Sub | Result],
      current = undefined
    }.

%% -------------------------------------------------------------------
%% Template name.
%% -------------------------------------------------------------------

domain(#var{prefix = undefined, name = "VHOST_NAME"})     -> "request";
domain(#var{prefix = undefined, name = "PATH"})           -> "request";
domain(#var{prefix = undefined, name = "CLIENT_IP"})      -> "request";
domain(#var{prefix = undefined, name = "CLIENT_PORT"})    -> "request";
domain(#var{prefix = undefined, name = "CLIENT_COUNTRY"}) -> "request";
domain(#var{prefix = undefined, name = "SERVER_PORT"})    -> "request";
domain(#var{prefix = undefined, name = "METHOD"})         -> "request";
domain(#var{prefix = undefined, name = "SCHEME"})         -> "request";
domain(#var{prefix = undefined, name = "HOST"})           -> "request";
domain(#var{prefix = undefined, name = "QUERY"})          -> "request";
domain(#var{prefix = undefined, name = "FRAGMENT"})       -> "request";
domain(#var{prefix = undefined, name = "AUTH_USER"})      -> "request";
domain(#var{prefix = undefined, name = "AUTH_PASSWD"})    -> "request";
domain(#var{prefix = undefined})                          -> "user";
domain(#var{prefix = Prefix})                             -> Prefix.

state_member(#var{prefix = undefined, name = "VHOST_NAME"})  -> "vhost_name";
state_member(#var{prefix = undefined, name = "PATH"})        -> "path";
state_member(#var{prefix = undefined, name = "CLIENT_IP"})   -> "client_ip";
state_member(#var{prefix = undefined, name = "CLIENT_PORT"}) -> "client_port";
state_member(#var{prefix = undefined, name = "SERVER_PORT"}) -> "server_port";
state_member(#var{prefix = undefined, name = "METHOD"})      -> "method";
state_member(#var{prefix = undefined, name = "SCHEME"})      -> "scheme";
state_member(#var{prefix = undefined, name = "HOST"})        -> "host";
state_member(#var{prefix = undefined, name = "QUERY"})       -> "query_str";
state_member(#var{prefix = undefined, name = "FRAGMENT"})    -> "fragment";
state_member(#var{prefix = undefined, name = "AUTH_USER"})   -> "auth_user";
state_member(#var{prefix = undefined, name = "AUTH_PASSWD"}) -> "auth_passwd";
state_member(#var{prefix = undefined})                       -> "vars";
state_member(_)                                              -> undefined.

expected_type(#var{prefix = undefined, name = "VHOST_NAME"})     -> string;
expected_type(#var{prefix = undefined, name = "PATH"})           -> string;
expected_type(#var{prefix = undefined, name = "CLIENT_IP"})      -> ipaddr;
expected_type(#var{prefix = undefined, name = "CLIENT_PORT"})    -> port;
expected_type(#var{prefix = undefined, name = "CLIENT_COUNTRY"}) -> string;
expected_type(#var{prefix = undefined, name = "HOST"})           -> string;
expected_type(#var{prefix = undefined, name = "SERVER_PORT"})    -> port;
expected_type(#var{prefix = undefined, name = "METHOD"})         -> string;
expected_type(#var{prefix = undefined, name = "SCHEME"})         -> string;
expected_type(#var{prefix = undefined, name = "QUERY"})          -> string;
expected_type(#var{prefix = undefined, name = "FRAGMENT"})       -> string;
expected_type(#var{prefix = undefined, name = "AUTH_USER"})      -> string;
expected_type(#var{prefix = undefined, name = "AUTH_PASSWD"})    -> string;
expected_type(_)                                                 -> undefined.

is_var([C | _])       when is_integer(C)     -> false;
is_var([[C | _] | _]) when is_integer(C)     -> true;
is_var(V)             when is_record(V, var) -> true;
is_var([V | _])       when is_record(V, var) -> true;
is_var(_)                                    -> false.

%% -------------------------------------------------------------------
%% String and variables expansion.
%% -------------------------------------------------------------------

expand_consts(#code{ruleset = Ruleset, current = Rule}, String) ->
    Consts = Rule#rule.consts,
    Global_Consts = Ruleset#ruleset.consts,
    expand_consts(Consts, Global_Consts, String).

expand_consts(_, _, [C | _] = String) when is_integer(C) ->
    %% Nothing to expand.
    String;
expand_consts(Consts, Global_Consts,
  #var{prefix = undefined, name = Name} = Var) ->
    %% Get const value.
    try
        dict:fetch(Name, Consts)
    catch
        error:badarg ->
            try
                dict:fetch(Name, Global_Consts)
            catch
                error:badarg ->
                    Var
            end
    end;
expand_consts(Consts, Global_Consts, Parts) when is_list(Parts) ->
    expand_consts_parts(Consts, Global_Consts, Parts, "");
expand_consts(_, _, Not_String) when not is_list(Not_String) ->
    Not_String.

expand_consts_parts(Consts, Global_Consts, [Part | Rest], Result) ->
    Part_S  = expand_consts(Consts, Global_Consts, Part),
    Result1 = case Result of
        "" ->
            %% Result is empty.
            case Part_S of
                [C2 | _] when is_integer(C2) -> Part_S;
                _ when is_list(Part_S)       -> Part_S;
                _                            -> [Part_S]
            end;
        [C1 | _] when is_integer(C1) ->
            %% Result is a string.
            case Part_S of
                [C2 | _] when is_integer(C2) -> Result ++ Part_S;
                _ when is_list(Part_S)       -> [Result] ++ Part_S;
                _                            -> [Result, Part_S]
            end;
        _ when is_list(Result) ->
            %% Result is a list.
            case Part_S of
                [C2 | _] when is_integer(C2) -> Result ++ [Part_S];
                _ when is_list(Part_S)       -> Result ++ Part_S;
                _                            -> Result ++ [Part_S]
            end;
        _ ->
            %% Type of Result is unknown.
            case Part_S of
                [C2 | _] when is_integer(C2) -> [Result, Part_S];
                _ when is_list(Part_S)       -> [Result] ++ Part_S;
                _                            -> [Result, Part_S]
            end
    end,
    expand_consts_parts(Consts, Global_Consts, Rest, Result1);
expand_consts_parts(_, _, [], Result) ->
    Result.

expand(State, [C | _] = String) when is_integer(C) ->
    %% Nothing to expand.
    {State, String};
expand(State, Var) when is_record(Var, var) ->
    %% Get variable value (as string).
    case get(State, Var) of
        {State1, undefined} -> {State1, ""};
        Ret                 -> Ret
    end;
expand(State, Parts) when is_list(Parts) ->
    expand_parts(State, Parts, "");
expand(State, Not_String) when not is_list(Not_String) ->
    {State, Not_String}.

expand_parts(State, [Part | Rest], Result) ->
    {State1, Part_S} = expand(State, Part),
    Result1 = Result ++ Part_S,
    expand_parts(State1, Rest, Result1);
expand_parts(State, [], Result) ->
    {State, lists:flatten(Result)}.

%% -------------------------------------------------------------------
%% Variable accessors.
%% -------------------------------------------------------------------

get(State, Var) ->
    get(State, Var, undefined).

get(State, Var, Type_Mod) ->
    {State1, Value} = get2(State, Var, Type_Mod),
    run_filters(State1, Var, Value, Type_Mod).

get2(#state{vhost_name = Value} = State,
  #var{prefix = undefined, name = "VHOST_NAME"}, undefined) ->
    {State, Value};
get2(#state{path = Value} = State,
  #var{prefix = undefined, name = "PATH"}, undefined) ->
    {State, Value};
get2(#state{client_ip = Value_C} = State,
  #var{prefix = undefined, name = "CLIENT_IP"}, undefined) ->
    Value_S = otis_type_ipaddr:to_string(Value_C),
    {State, Value_S};
get2(#state{client_ip = Value_C} = State,
  #var{prefix = undefined, name = "CLIENT_IP"}, otis_type_ipaddr) ->
    {State, Value_C};
get2(#state{client_port = Value_C} = State,
  #var{prefix = undefined, name = "CLIENT_PORT"}, undefined) ->
    Value_S = otis_type_port:to_string(Value_C),
    {State, Value_S};
get2(#state{client_port = Value_C} = State,
  #var{prefix = undefined, name = "CLIENT_PORT"}, otis_type_port) ->
    {State, Value_C};
get2(#state{client_country = undefined, client_ip = undefined} = State,
  #var{prefix = undefined, name = "CLIENT_COUNTRY"}, undefined) ->
    State1 = State#state{
      client_country = ""
    },
    {State1, ""};
get2(#state{client_country = undefined, client_ip = IP_Addr} = State,
  #var{prefix = undefined, name = "CLIENT_COUNTRY"}, undefined) ->
    Value = case otis_geoip:get_country_iso_code(IP_Addr) of
        undefined -> "";
        V         -> V
    end,
    State1 = State#state{
      client_country = Value
    },
    {State1, Value};
get2(#state{client_country = Value} = State,
  #var{prefix = undefined, name = "CLIENT_COUNTRY"}, undefined) ->
    {State, Value};
get2(#state{server_port = Value_C} = State,
  #var{prefix = undefined, name = "SERVER_PORT"}, undefined) ->
    Value_S = otis_type_port:to_string(Value_C),
    {State, Value_S};
get2(#state{server_port = Value_C} = State,
  #var{prefix = undefined, name = "SERVER_PORT"}, otis_type_port) ->
    {State, Value_C};
get2(#state{method = Value} = State,
  #var{prefix = undefined, name = "METHOD"}, undefined) ->
    {State, Value};
get2(#state{scheme = Value} = State,
  #var{prefix = undefined, name = "SCHEME"}, undefined) ->
    {State, Value};
get2(#state{host = Value} = State,
  #var{prefix = undefined, name = "HOST"}, undefined) ->
    {State, Value};
get2(#state{query_parsed = false, query_str = Value} = State,
  #var{prefix = undefined, name = "QUERY"},
  undefined) ->
    {State, Value};
get2(State, #var{prefix = undefined, name = "QUERY"} = Var, Type_Mod) ->
    State1 = otis_utils:stringify_query(State),
    get2(State1, Var, Type_Mod);
get2(#state{fragment = Value} = State,
  #var{prefix = undefined, name = "FRAGMENT"}, undefined) ->
    {State, Value};
get2(#state{auth_user = Value} = State,
  #var{prefix = undefined, name = "AUTH_USER"}, undefined) ->
    {State, Value};
get2(#state{auth_passwd = Value} = State,
  #var{prefix = undefined, name = "AUTH_PASSWD"}, undefined) ->
    {State, Value};
get2(State, #var{prefix = undefined, name = "URI"}, undefined) ->
    otis_utils:format_uri(State);
get2(#state{rule_name = Rule_Name} = State,
  #var{prefix = undefined, name = "RULE"}, undefined) ->
    {State, Rule_Name};
get2(State,
  #var{prefix = undefined, name = "STATE"}, undefined) ->
    {State, lists:flatten(io_lib:format("~p", [State]))};
get2(
  #state{method = Method,
    response = Is_Resp, code = Code, reason = Reason,
    rheaders = RHeaders, http_ver = {Maj, Min}} = State,
  #var{prefix = undefined, name = "RESULT"}, undefined) ->
    if
        Is_Resp ->
            %% HTTP response.
            RHeaders1 = otis_utils:format_headers(RHeaders),
            Req = io_lib:format(
              "HTTP/~b.~b ~b ~s\r\n"
              "~s"
              "\r\n",
              [Maj, Min, Code, Reason, RHeaders1]),
            {State, Req};
        true ->
            %% HTTP request.
            {State1, Path} = otis_utils:rebuild_path(State),
            State2   = cookies_to_headers(State1),
            Headers  = State2#state.headers,
            Headers1 = otis_utils:format_headers(Headers),
            Req = io_lib:format(
              "~s ~s HTTP/~b.~b\r\n"
              "~s"
              "\r\n",
              [Method, Path, Maj, Min, Headers1]),
            {State1, Req}
    end;
get2(#state{vars = Vars} = State,
  #var{prefix = undefined, name = "USER_VARS"}, undefined) ->
    List1 = lists:keysort(1, dict:to_list(Vars)),
    Max_Fun = fun({N, _}, M) ->
        L = length(N),
        if
            L > M -> L;
            true  -> M
        end
    end,
    Max    = lists:foldl(Max_Fun, 0, List1),
    Format = lists:flatten(io_lib:format("~~-~bs = ~~s~n", [Max])),
    List2  = [
      begin
          V = case VS of
              undefined -> TM:to_string(VC);
              _         -> VS
          end,
          io_lib:format(Format, [N, V])
      end
      || {_, {N, VS, TM, VC}} <- List1
    ],
    {State, lists:flatten(List2)};
get2(State, #var{name = [C | _] = Name} = Var, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    {State1, Name1} = expand(State, Name),
    Var1  = Var#var{
      name = Name1
    },
    get2(State1, Var1, Type_Mod);
get2(State, #var{prefix = undefined, name = Name}, Type_Mod) ->
    get_uservar(State, Name, Type_Mod);
get2(State, #var{prefix = "query", name = Name}, Type_Mod) ->
    get_query(State, Name, Type_Mod);
get2(State, #var{prefix = "header", name = Name}, Type_Mod) ->
    get_header(State, Name, Type_Mod);
get2(State, #var{prefix = "rheader", name = Name}, Type_Mod) ->
    get_rheader(State, Name, Type_Mod);
get2(State, #var{prefix = "cookie", name = Name}, Type_Mod) ->
    get_cookie(State, Name, Type_Mod).

get_uservar(State, Name) ->
    get_uservar(State, Name, undefined).

get_uservar(State, [C | _] = Name, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    {State1, Name1} = expand(State, Name),
    get_uservar(State1, Name1, Type_Mod);
get_uservar(#state{vars = Vars} = State, Name, undefined) ->
    try
        case dict:fetch(Name, Vars) of
            {_, undefined, Type_Mod, Value_C} ->
                Value_S = Type_Mod:to_string(Value_C),
                Vars1   = dict:store(Name,
                  {Name, Value_S, Type_Mod, Value_C},
                  Vars),
                State1 = State#state{
                  vars = Vars1
                },
                {State1, Value_S};
            {_, Value_S, _, _} ->
                {State, Value_S}
        end
    catch
        error:badarg ->
            %% Undefined variable.
            {State, undefined}
    end;
get_uservar(#state{vars = Vars} = State, Name, Type_Mod) ->
    try
        case dict:fetch(Name, Vars) of
            {_, _, Type_Mod, Value_C} ->
                {State, Value_C};
            {_, Value_S0, Other_Type_Mod, Value_C0} ->
                Value_S = case Value_S0 of
                    undefined -> Other_Type_Mod:to_string(Value_C0);
                    _         -> Value_S0
                end,
                %% FIXME: How to handle conversion failure?
                Value_C = Type_Mod:from_string(Value_S),
                Vars1 = dict:store(Name,
                  {Name, Value_S, Type_Mod, Value_C},
                  Vars),
                State1 = State#state{
                  vars = Vars1
                },
                {State1, Value_C}
        end
    catch
        error:badarg ->
            %% Undefined variable.
            {State, undefined}
    end.

get_query(State, Name) ->
    get_query(State, Name, undefined).

get_query(State, [C | _] = Name, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    {State1, Name1} = expand(State, Name),
    get_query(State1, Name1, Type_Mod);
get_query(#state{query_parsed = false} = State, Name, Type_Mod) ->
    State1 = otis_utils:parse_query(State),
    get_query(State1, Name, Type_Mod);
get_query(#state{query_str = Query} = State, Name, Type_Mod) ->
    case get_loop(Name, Type_Mod, Query, [], undefined, false) of
        {Value, Query1} ->
            State1 = State#state{
              query_str = Query1
            },
            {State1, Value};
        Value ->
            {State, Value}
    end.

get_header(State, Name) ->
    get_header(State, Name, undefined).

get_header(State, [C | _] = Name, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    {State1, Name1} = expand(State, Name),
    get_header(State1, Name1, Type_Mod);
get_header(State, Name, Type_Mod) ->
    Name1 = string:to_lower(Name),
    get_header_lc(State, Name1, Type_Mod).

get_header_lc(#state{cookies = Cookies} = State, "cookie", Type_Mod)
  when Cookies /= undefined ->
    Value_S = otis_utils:format_cookie_header(Cookies),
    case Type_Mod of
        undefined ->
            {State, Value_S};
        _ ->
            Value_C = Type_Mod:from_string(Value_S),
            {State, Value_C}
    end;
get_header_lc(#state{headers = Headers} = State, Name, Type_Mod) ->
    case get_loop(Name, Type_Mod, Headers, [], undefined, false) of
        {Value, Headers1} ->
            State1 = State#state{
              headers = Headers1
            },
            {State1, Value};
        Value ->
            {State, Value}
    end.

get_rheader(State, Name) ->
    get_rheader(State, Name, undefined).

get_rheader(State, [C | _] = Name, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    {State1, Name1} = expand(State, Name),
    get_rheader(State1, Name1, Type_Mod);
get_rheader(State, Name, Type_Mod) ->
    Name1 = string:to_lower(Name),
    get_rheader_lc(State, Name1, Type_Mod).

get_rheader_lc(#state{rheaders = Headers} = State, Name, Type_Mod) ->
    case get_loop(Name, Type_Mod, Headers, [], undefined, false) of
        {Value, Headers1} ->
            State1 = State#state{
              rheaders = Headers1
            },
            {State1, Value};
        Value ->
            {State, Value}
    end.

get_cookie(State, Name) ->
    get_cookie(State, Name, undefined).

get_cookie(State, [C | _] = Name, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    {State1, Name1} = expand(State, Name),
    get_header(State1, Name1, Type_Mod);
get_cookie(#state{cookies = undefined} = State, Name, Type_Mod) ->
    %% Cookies are not parsed yet: get the "Cookie" headers and parse
    %% their value.
    State1 = headers_to_cookies(State),
    get_cookie(State1, Name, Type_Mod);
get_cookie(#state{cookies = Cookies} = State, Name, Type_Mod) ->
    case get_loop(Name, Type_Mod, Cookies, [], undefined, false) of
        {Value, Cookies1} ->
            State1 = State#state{
              cookies = Cookies1
            },
            {State1, Value};
        Value ->
            {State, Value}
    end.

headers_to_cookies(#state{headers = Headers} = State) ->
    Ret = get_loop("cookie", undefined, Headers, [], [], remove),
    {Cookie_Hds1, Headers2} = case Ret of
        {Cookie_Hds, Headers1} -> {Cookie_Hds, Headers1};
        Cookie_Hds             -> {Cookie_Hds, Headers}
    end,
    Cookies = parse_cookie_hds(Cookie_Hds1, []),
    State#state{
      headers = Headers2,
      cookies = Cookies
    }.

cookies_to_headers(#state{cookies = undefined} = State) ->
    State;
cookies_to_headers(#state{cookies = []} = State) ->
    State#state{
      cookies = undefined
    };
cookies_to_headers(#state{cookies = Cookies, headers = Headers} = State) ->
    Cookie_Hd = otis_utils:format_cookie_header(Cookies),
    Headers1 = Headers ++ [{"cookie", Cookie_Hd, undefined, undefined}],
    State#state{
      headers = Headers1,
      cookies = undefined
    }.

parse_cookie_hds([Cookie_Hd | Rest], All_Cookies) ->
    Cookies = otis_utils:parse_cookie_header(Cookie_Hd),
    parse_cookie_hds(Rest, All_Cookies ++ Cookies);
parse_cookie_hds([], All_Cookies) ->
    All_Cookies.

get_loop(Name, undefined,
  [{Name, Value, _, _} = Item | Rest] = List_Tail,
  List_Head, Values, Converted) when Value /= undefined ->
    %% Item's value is already a string and that's what the caller
    %% requested.
    case Values of
        undefined when Converted == remove ->
            get_loop2(Value, List_Head, List_Tail, Converted);
        undefined ->
            get_loop2(Value, List_Head, Rest, Converted);
        _ when Converted == remove ->
            get_loop(Name, undefined, Rest, List_Head,
              [Value | Values], Converted);
        _ ->
            get_loop(Name, undefined, Rest, [Item | List_Head],
              [Value | Values], Converted)
    end;
get_loop(Name, undefined,
  [{Name, undefined, Type_Mod, Value_C} | List_Tail],
  List_Head, Values, Converted) ->
    %% Item's value must be converted first before we can return its
    %% value.
    Value_S = Type_Mod:to_string(Value_C),
    Item1   = {Name, Value_S, Type_Mod, Value_C},
    case Values of
        undefined when Converted == remove ->
            get_loop2(Value_S, List_Head, List_Tail, Converted);
        undefined ->
            get_loop2(Value_S, [Item1 | List_Head], List_Tail, true);
        _ when Converted == remove ->
            get_loop(Name, undefined, List_Tail, List_Head,
              [Value_S | Values], Converted);
        _ ->
            get_loop(Name, undefined, List_Tail, [Item1 | List_Head],
              [Value_S | Values], true)
    end;
get_loop(Name, Type_Mod,
  [{Name, _, Type_Mod, Value_C} = Item | Rest] = List_Tail,
  List_Head, Values, Converted) when Type_Mod /= undefined ->
    %% Item's value is compatible with Type_Mod, we can return it
    %% without conversion.
    case Values of
        undefined when Converted == remove ->
            get_loop2(Value_C, List_Head, List_Tail, Converted);
        undefined ->
            get_loop2(Value_C, List_Head, List_Tail, Converted);
        _ when Converted == remove ->
            get_loop(Name, Type_Mod, Rest, List_Head,
              [Value_C | Values], Converted);
        _ ->
            get_loop(Name, Type_Mod, Rest, [Item | List_Head],
              [Value_C | Values], Converted)
    end;
get_loop(Name, Type_Mod,
  [{Name, Value_S0, Other_Type_Mod, Value_C0} = Item | List_Tail],
  List_Head, Values, Converted) when Type_Mod /= undefined ->
    %% Item's value must be converted first before we can return its
    %% value.
    Value_S = case Value_S0 of
        undefined -> Other_Type_Mod:to_string(Value_C0);
        _         -> Value_S0
    end,
    try
        Value_C = Type_Mod:from_string(Value_S),
        Item1   = {Name, Value_S, Type_Mod, Value_C},
        case Values of
            undefined when Converted == remove ->
                get_loop2(Value_C, List_Head, List_Tail, Converted);
            undefined ->
                get_loop2(Value_C, [Item1 | List_Head], List_Tail, true);
            _ when Converted == remove ->
                get_loop(Name, Type_Mod, List_Tail, List_Head,
                  [Value_C | Values], Converted);
            _ ->
                get_loop(Name, Type_Mod, List_Tail, [Item1 | List_Head],
                  [Value_C | Values], true)
        end
    catch
        throw:conversion_failed ->
            %% Item's value can't be converted. Return "undefined".
            case Values of
                undefined when Converted == remove ->
                    get_loop2(undefined, List_Head, List_Tail,
                      Converted);
                undefined ->
                    get_loop2(undefined, [Item | List_Head], List_Tail,
                      Converted);
                _ when Converted == remove ->
                    get_loop(Name, Type_Mod, List_Tail, List_Head,
                      Values, Converted);
                _ ->
                    get_loop(Name, Type_Mod, List_Tail, [Item | List_Head],
                      Values, Converted)
            end
    end;
get_loop(Name, Type_Mod, [Item | List_Tail], List_Head,
  Values, Converted) ->
    get_loop(Name, Type_Mod, List_Tail, [Item | List_Head],
      Values, Converted);
get_loop(_, _, [], Query, Values, Converted) ->
    get_loop2(Values, Query, [], Converted).

get_loop2(Value, _, _, false) ->
    Value;
get_loop2(Value, List_Head, List_Tail, _) ->
    List1 = lists:reverse(List_Head) ++ List_Tail,
    {Value, List1}.

set(State, Var, Value) ->
    set(State, Var, Value, undefined).

set(State, #var{prefix = undefined, name = "PATH"}, Value, undefined) ->
    State#state{
      path = Value
    };
set(State, #var{prefix = undefined, name = "CLIENT_IP"}, Value_S, undefined) ->
    Value_C = otis_type_ipaddr:from_string(Value_S),
    State#state{
      client_ip = Value_C,
      client_country = undefined
    };
set(State, #var{prefix = undefined, name = "CLIENT_IP"}, Value_C,
  otis_type_ipaddr) ->
    State#state{
      client_ip = Value_C,
      client_country = undefined
    };
set(State, #var{prefix = undefined, name = "CLIENT_COUNTRY"}, Value,
  undefined) ->
    State#state{
      client_country = Value
    };
set(State, #var{prefix = undefined, name = "METHOD"}, Value, undefined) ->
    State#state{
      method = Value
    };
set(State, #var{prefix = undefined, name = "SCHEME"}, Value, undefined) ->
    State#state{
      scheme = Value
    };
set(State, #var{prefix = undefined, name = "HOST"}, Value, undefined) ->
    State#state{
      host = Value
    };
set(State, #var{prefix = undefined, name = "QUERY"}, Value, undefined) ->
    State#state{
      query_str    = Value,
      query_parsed = false
    };
set(State, #var{prefix = undefined, name = "FRAGMENT"}, Value, undefined) ->
    State#state{
      fragment = Value
    };
set(State, #var{prefix = undefined, name = "AUTH_USER"}, Value, undefined) ->
    State#state{
      auth_user = Value
    };
set(State, #var{prefix = undefined, name = "AUTH_PASSWD"}, Value, undefined) ->
    State#state{
      auth_passwd = Value
    };
set(State, #var{prefix = undefined, name = "URI"}, Value, undefined) ->
    case otis_utils:parse_uri(Value) of
        {Scheme, Host, Port, Path, Query} ->
            Host1 = otis_utils:format_host(Scheme, Host, Port),
            State1 = State#state{
              host         = Host,
              server_ip    = undefined,
              server_port  = Port,
              scheme       = Scheme,
              path         = Path,
              query_str    = Query,
              query_parsed = false
            },
            set_header(State1, "host", Host1);
        undefined ->
            ?ERROR("Invalid $(URI): ~p~n", [Value]),
            State
    end;
set(State, #var{name = Name} = Var, _, _) when not ?IS_VAR_WRITABLE(Var) ->
    ?WARNING("Tried to set the read-only variable \"~s\".~n", [Name]),
    State;
set(State, #var{name = [C | _] = Name} = Var, Value, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    {State1, Name1} = expand(State, Name),
    Var1  = Var#var{
      name = Name1
    },
    set(State1, Var1, Value, Type_Mod);
set(State, #var{prefix = undefined, name = Name}, Value, Type_Mod) ->
    set_uservar(State, Name, Value, Type_Mod);
set(State, #var{prefix = "query", name = Name}, Value, Type_Mod) ->
    set_query(State, Name, Value, Type_Mod);
set(State, #var{prefix = "header", name = Name}, Value, Type_Mod) ->
    set_header(State, Name, Value, Type_Mod);
set(State, #var{prefix = "rheader", name = Name}, Value, Type_Mod) ->
    set_rheader(State, Name, Value, Type_Mod);
set(State, #var{prefix = "cookie", name = Name}, Value, Type_Mod) ->
    set_cookie(State, Name, Value, Type_Mod).

set_uservar(State, Name, Value) ->
    set_uservar(State, Name, Value, undefined).

set_uservar(State, [C | _] = Name, Value, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    {State1, Name1} = expand(State, Name),
    set_uservar(State1, Name1, Value, Type_Mod);
set_uservar(#state{vars = Vars} = State, Name, "", undefined) ->
    Vars1 = dict:erase(Name, Vars),
    State#state{
      vars = Vars1
    };
set_uservar(#state{vars = Vars} = State, Name, Value, undefined) ->
    Vars1 = dict:store(Name, {Name, Value, undefined, undefined}, Vars),
    State#state{
      vars = Vars1
    };
set_uservar(#state{vars = Vars} = State, Name, Value, Type_Mod) ->
    Vars1 = dict:store(Name, {Name, undefined, Type_Mod, Value}, Vars),
    State#state{
      vars = Vars1
    }.

set_query(State, Name, Value) ->
    set_query(State, Name, Value, undefined).

set_query(State, [C | _] = Name, Value, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    {State1, Name1} = expand(State, Name),
    set_query(State1, Name1, Value, Type_Mod);
set_query(#state{query_parsed = false} = State, Name, Value, Type_Mod) ->
    State1 = otis_utils:parse_query(State),
    set_query(State1, Name, Value, Type_Mod);
set_query(#state{query_str = Query} = State, Name, Value, undefined) ->
    Query1 = case Value of
        "" ->
            unset_loop(Name, Query);
        _ ->
            Param = {Name, Value, undefined, undefined},
            set_loop(Name, Param, Query)
    end,
    State#state{
      query_str = Query1
    };
set_query(#state{query_str = Query} = State, Name, Value, Type_Mod) ->
    Param = {Name, undefined, Type_Mod, Value},
    Query1 = set_loop(Name, Param, Query),
    State#state{
      query_str = Query1
    }.

set_header(State, Name, Value) ->
    set_header(State, Name, Value, undefined).

set_header(State, [C | _] = Name, Value, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    {State1, Name1} = expand(State, Name),
    set_header(State1, Name1, Value, Type_Mod);
set_header(#state{headers = Headers} = State, Name, Value, undefined) ->
    Name1 = string:to_lower(Name),
    Headers1 = case Value of
        "" ->
            unset_loop(Name1, Headers);
        _ ->
            Param = {Name1, Value, undefined, undefined},
            set_loop(Name1, Param, Headers)
    end,
    case Name1 of
        "cookie" ->
            State#state{
              headers = Headers1,
              cookies = undefined
            };
        _ ->
            State#state{
              headers = Headers1
            }
    end;
set_header(#state{headers = Headers} = State, Name, Value, Type_Mod) ->
    Name1 = string:to_lower(Name),
    Param = {Name1, undefined, Type_Mod, Value},
    Headers1 = set_loop(Name1, Param, Headers),
    case Name1 of
        "cookie" ->
            State#state{
              headers = Headers1,
              cookies = undefined
            };
        _ ->
            State#state{
              headers = Headers1
            }
    end.

set_rheader(State, Name, Value) ->
    set_rheader(State, Name, Value, undefined).

set_rheader(State, [C | _] = Name, Value, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    {State1, Name1} = expand(State, Name),
    set_rheader(State1, Name1, Value, Type_Mod);
set_rheader(#state{rheaders = Headers} = State, Name, Value, undefined) ->
    Name1 = string:to_lower(Name),
    Headers1 = case Value of
        "" ->
            unset_loop(Name1, Headers);
        _ ->
            Param = {Name1, Value, undefined, undefined},
            set_loop(Name1, Param, Headers)
    end,
    State#state{
      rheaders = Headers1
    };
set_rheader(#state{rheaders = Headers} = State, Name, Value, Type_Mod) ->
    Name1 = string:to_lower(Name),
    Param = {Name1, undefined, Type_Mod, Value},
    Headers1 = set_loop(Name1, Param, Headers),
    State#state{
      rheaders = Headers1
    }.

set_cookie(State, Name, Value) ->
    set_cookie(State, Name, Value, undefined).

set_cookie(State, [C | _] = Name, Value, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    {State1, Name1} = expand(State, Name),
    set_cookie(State1, Name1, Value, Type_Mod);
set_cookie(#state{cookies = undefined} = State, Name, Value, Type_Mod) ->
    %% Cookies are not parsed yet: get the "Cookie" headers and parse
    %% their value.
    State1 = headers_to_cookies(State),
    set_cookie(State1, Name, Value, Type_Mod);
set_cookie(#state{cookies = Cookies} = State, Name, Value, undefined) ->
    Cookies1 = case Value of
        "" ->
            unset_loop(Name, Cookies);
        _ ->
            Param = {Name, Value, undefined, undefined},
            set_loop(Name, Param, Cookies)
    end,
    State#state{
      cookies = Cookies1
    };
set_cookie(#state{cookies = Cookies} = State, Name, Value, Type_Mod) ->
    Param = {Name, undefined, Type_Mod, Value},
    Cookies1 = set_loop(Name, Param, Cookies),
    State#state{
      cookies = Cookies1
    }.

set_loop(Name, New_Item, List) ->
    set_loop2(Name, New_Item, List, [], false).

set_loop2(Name, New_Item, [{Name, _, _, _} | Rest], List, false) ->
    %% The name matches, store the new value.
    set_loop2(Name, New_Item, Rest, [New_Item | List], true);
set_loop2(Name, New_Item, [{Name, _, _, _} | Rest], List, true) ->
    %% The name matches but the value was previously set. Remove this
    %% following value.
    set_loop2(Name, New_Item, Rest, List, true);
set_loop2(Name, New_Item, [Item | Rest], List, Already_Set) ->
    set_loop2(Name, New_Item, Rest, [Item | List], Already_Set);
set_loop2(_, New_Item, [], List, false) ->
    List1 = [New_Item | List],
    lists:reverse(List1);
set_loop2(_, _, [], List, true) ->
    lists:reverse(List).

unset_loop(Name, List) ->
    unset_loop2(Name, List, []).

unset_loop2(Name, [{Name, _, _, _} | Rest], List) ->
    unset_loop2(Name, Rest, List);
unset_loop2(Name, [Item | Rest], List) ->
    unset_loop2(Name, Rest, [Item | List]);
unset_loop2(_, [], List) ->
    lists:reverse(List).

%% -------------------------------------------------------------------
%% Filters.
%% -------------------------------------------------------------------

run_filters(State, Var, Value) ->
    run_filters(State, Var, Value, undefined).

run_filters(State, #var{filters = Filters}, Value, Type_Mod) ->
    run_filters2(State, Filters, Value, Type_Mod).

run_filters2(State, ["escape_uri" | Rest], Value, undefined) ->
    Value1 = otis_utils:escape_uri(Value),
    run_filters2(State, Rest, Value1, undefined);
run_filters2(State, ["unescape_uri" | Rest], Value, undefined) ->
    Value1 = otis_utils:unescape_uri(Value),
    run_filters2(State, Rest, Value1, undefined);
run_filters2(State, [Filter | Rest], Value, Type_Mod) ->
    ?WARNING("Unsupported filter (with type ~s): ~p~n", [Type_Mod, Filter]),
    run_filters2(State, Rest, Value, Type_Mod);
run_filters2(State, [], Value, _) ->
    {State, Value}.
