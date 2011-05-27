-module(otis_var).

-include("otis.hrl").
-include("otis_codegen.hrl").

%% Public API.
-export([
    prepare_string/1,
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
    set/3,
    set/4,
    set_uservar/3,
    set_uservar/4,
    set_query/3,
    set_query/4,
    set_header/3,
    set_header/4,
    set_rheader/3,
    set_rheader/4
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

prepare_string(Not_String) when not is_list(Not_String) ->
    Not_String;
prepare_string(String) ->
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
        [R] when is_list(R) -> R;
        _                   -> Result1
    end;
find_variables([], Parser) ->
    %% Collapse one level.
    Parser1 = collapse(Parser),
    find_variables([], Parser1).

%% Reverse text or create final variable.
collapse(#parser{current = undefined} = Parser) ->
    Parser;
collapse(#parser{current = #var_st{name = Name}} = Parser) ->
    %% Extract attribute.
    [Tail | Rest] = Name,
    {Tail1, Attr} = if
        is_list(Tail) ->
            case string:rchr(Tail, $#) of
                0 ->
                    {Tail, undefined};
                I1 ->
                    {
                      string:substr(Tail, 1, I1 - 1),
                      string:substr(Tail, I1 + 1)
                    }
            end;
        true ->
            {Tail, undefined}
    end,
    if
        Attr == "" ->
            ?ERROR("Invalid empty variable attribute~n", []),
            throw(invalid_variable_attribute);
        true ->
            ok
    end,
    %% Extract prefix.
    [Head | Rest1] = case Tail1 of
        "" -> lists:reverse(Rest);
        _  -> lists:reverse([Tail1 | Rest])
    end,
    {Prefix, Head1} = if
        is_list(Head) ->
            case string:chr(Head, $:) of
                0 ->
                    {undefined, Head};
                I2 ->
                    {
                      string:substr(Head, 1, I2 - 1),
                      string:substr(Head, I2 + 1)
                    }
            end;
        true ->
            {undefined, Head}
    end,
    if
        Prefix == "" ->
            ?ERROR("Invalid empty variable prefix~n", []),
            throw(invalid_variable_prefix);
        true ->
            ok
    end,
    Name1 = case Head1 of
        "" -> Rest1;
        _  -> [Head1 | Rest1]
    end,
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
      prefix = Prefix,
      name   = Name2,
      attr   = Attr
    },
    collapse2(Parser, Var);
collapse(#parser{current = Text} = Parser) ->
    collapse2(Parser, lists:reverse(Text)).

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

domain(#var{prefix = undefined, name = "PATH"})        -> "request";
domain(#var{prefix = undefined, name = "CLIENT_PORT"}) -> "request";
domain(#var{prefix = undefined, name = "SERVER_NAME"}) -> "request";
domain(#var{prefix = undefined, name = "SERVER_PORT"}) -> "request";
domain(#var{prefix = undefined, name = "METHOD"})      -> "request";
domain(#var{prefix = undefined, name = "SCHEME"})      -> "request";
domain(#var{prefix = undefined, name = "QUERY"})       -> "request";
domain(#var{prefix = undefined, name = "FRAGMENT"})    -> "request";
domain(#var{prefix = undefined})                       -> "user";
domain(#var{prefix = Prefix})                          -> Prefix.

state_member(#var{prefix = undefined, name = "PATH"})        -> "path";
state_member(#var{prefix = undefined, name = "CLIENT_PORT"}) -> "client_port";
state_member(#var{prefix = undefined, name = "SERVER_NAME"}) -> "server_name";
state_member(#var{prefix = undefined, name = "SERVER_PORT"}) -> "server_port";
state_member(#var{prefix = undefined, name = "METHOD"})      -> "method";
state_member(#var{prefix = undefined, name = "SCHEME"})      -> "scheme";
state_member(#var{prefix = undefined, name = "QUERY"})       -> "query_str";
state_member(#var{prefix = undefined, name = "FRAGMENT"})    -> "fragment";
state_member(#var{prefix = undefined})                       -> "vars";
state_member(_)                                              -> undefined.

expected_type(#var{prefix = undefined, name = "PATH"})        -> string;
expected_type(#var{prefix = undefined, name = "CLIENT_PORT"}) -> port;
expected_type(#var{prefix = undefined, name = "SERVER_NAME"}) -> string;
expected_type(#var{prefix = undefined, name = "SERVER_PORT"}) -> port;
expected_type(#var{prefix = undefined, name = "METHOD"})      -> string;
expected_type(#var{prefix = undefined, name = "SCHEME"})      -> string;
expected_type(#var{prefix = undefined, name = "QUERY"})       -> string;
expected_type(#var{prefix = undefined, name = "FRAGMENT"})    -> string;
expected_type(_)                                              -> undefined.

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
    {State, Result}.

%% -------------------------------------------------------------------
%% Variable accessors.
%% -------------------------------------------------------------------

get(State, Var) ->
    get(State, Var, undefined).

get(#state{path = Value} = State,
  #var{prefix = undefined, name = "PATH"}, undefined) ->
    {State, Value};
get(#state{client_port = Value_C} = State,
  #var{prefix = undefined, name = "CLIENT_PORT"}, undefined) ->
    Value_S = otis_type_port:to_string(Value_C),
    {State, Value_S};
get(#state{client_port = Value_C} = State,
  #var{prefix = undefined, name = "CLIENT_PORT"}, otis_type_port) ->
    {State, Value_C};
get(#state{server_name = Value} = State,
  #var{prefix = undefined, name = "SERVER_NAME"}, undefined) ->
    {State, Value};
get(#state{server_port = Value_C} = State,
  #var{prefix = undefined, name = "SERVER_PORT"}, undefined) ->
    Value_S = otis_type_port:to_string(Value_C),
    {State, Value_S};
get(#state{server_port = Value_C} = State,
  #var{prefix = undefined, name = "SERVER_PORT"}, otis_type_port) ->
    {State, Value_C};
get(#state{method = Value} = State,
  #var{prefix = undefined, name = "METHOD"}, undefined) ->
    {State, Value};
get(#state{scheme = Value} = State,
  #var{prefix = undefined, name = "SCHEME"}, undefined) ->
    {State, Value};
get(#state{query_parsed = false, query_str = Value} = State,
  #var{prefix = undefined, name = "QUERY"},
  undefined) ->
    {State, Value};
get(State, #var{prefix = undefined, name = "QUERY"} = Var, Type_Mod) ->
    State1 = otis_utils:stringify_query(State),
    get(State1, Var, Type_Mod);
get(#state{fragment = Value} = State,
  #var{prefix = undefined, name = "FRAGMENT"}, undefined) ->
    {State, Value};
get(State, #var{prefix = undefined, name = "URI"}, undefined) ->
    otis_utils:format_uri(State);
get(#state{rule_name = Rule_Name} = State,
  #var{prefix = undefined, name = "RULE"}, undefined) ->
    {State, Rule_Name};
get(State,
  #var{prefix = undefined, name = "STATE"}, undefined) ->
    {State, lists:flatten(io_lib:format("~p", [State]))};
get(
  #state{method = Method, headers = Headers,
    response = Is_Resp, code = Code, reason = Reason,
    rheaders = RHeaders, http_ver = {Maj, Min}} = State,
  #var{prefix = undefined, name = "REQUEST"}, undefined) ->
    if
        Is_Resp ->
            %% HTTP response.
            RHeaders1 = case otis_utils:format_headers(RHeaders) of
                ""  -> "";
                RH1 -> RH1 ++ "\n"
            end,
            Req = io_lib:format(
              "HTTP/~b.~b ~b ~s~n"
              "~s",
              [Maj, Min, Code, Reason, RHeaders1]),
            {State, Req};
        true ->
            %% HTTP request.
            {State1, Path} = otis_utils:rebuild_path(State),
            Headers1 = case otis_utils:format_headers(Headers) of
                "" -> "";
                H1 -> H1 ++ "\n"
            end,
            Req = io_lib:format(
              "~s ~s HTTP/~b.~b~n"
              "~s",
              [Method, Path, Maj, Min, Headers1]),
            {State1, Req}
    end;
get(#state{vars = Vars} = State,
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
get(State, #var{name = [C | _] = Name} = Var, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    Name1 = expand(State, Name),
    Var1  = Var#var{
      name = Name1
    },
    get(State, Var1, Type_Mod);
get(State, #var{prefix = undefined, name = Name}, Type_Mod) ->
    get_uservar(State, Name, Type_Mod);
get(State, #var{prefix = "query", name = Name}, Type_Mod) ->
    get_query(State, Name, Type_Mod);
get(State, #var{prefix = "header", name = Name}, Type_Mod) ->
    get_header(State, Name, Type_Mod);
get(State, #var{prefix = "rheader", name = Name}, Type_Mod) ->
    get_rheader(State, Name, Type_Mod).

get_uservar(State, Name) ->
    get_uservar(State, Name, undefined).

get_uservar(State, [C | _] = Name, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    Name1 = expand(State, Name),
    get_uservar(State, Name1, Type_Mod);
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
    Name1 = expand(State, Name),
    get_query(State, Name1, Type_Mod);
get_query(#state{query_parsed = false} = State, Name, Type_Mod) ->
    State1 = otis_utils:parse_query(State),
    get_query(State1, Name, Type_Mod);
get_query(#state{query_str = Query} = State, Name, Type_Mod) ->
    case get_loop(Name, Type_Mod, Query, [], false) of
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
    Name1 = expand(State, Name),
    get_header(State, Name1, Type_Mod);
get_header(#state{headers = Headers} = State, Name, Type_Mod) ->
    Name1 = string:to_lower(Name),
    case get_loop(Name1, Type_Mod, Headers, [], false) of
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
    Name1 = expand(State, Name),
    get_rheader(State, Name1, Type_Mod);
get_rheader(#state{rheaders = Headers} = State, Name, Type_Mod) ->
    Name1 = string:to_lower(Name),
    case get_loop(Name1, Type_Mod, Headers, [], false) of
        {Value, Headers1} ->
            State1 = State#state{
              rheaders = Headers1
            },
            {State1, Value};
        Value ->
            {State, Value}
    end.

get_loop(Name, undefined,
  [{Name, Value, _, _} | _] = List_Tail,
  List_Head, Converted) when Value /= undefined ->
    %% Item's value is already a string and that's what the caller
    %% requested.
    get_loop2(Value, List_Head, List_Tail, Converted);
get_loop(Name, undefined,
  [{Name, undefined, Type_Mod, Value_C} | List_Tail],
  List_Head, _) ->
    %% Item's value must be converted first before we can return its
    %% value.
    Value_S = Type_Mod:to_string(Value_C),
    Item1   = {Name, Value_S, Type_Mod, Value_C},
    get_loop2(Value_S, [Item1 | List_Head], List_Tail, true);
get_loop(Name, Type_Mod,
  [{Name, _, Type_Mod, Value_C} | _] = List_Tail,
  List_Head, Converted) when Type_Mod /= undefined ->
    %% Item's value is compatible with Type_Mod, we can return it
    %% without conversion.
    get_loop2(Value_C, List_Head, List_Tail, Converted);
get_loop(Name, Type_Mod,
  [{Name, Value_S0, Other_Type_Mod, Value_C0} = Item | List_Tail],
  List_Head, Converted) when Type_Mod /= undefined ->
    %% Item's value must be converted first before we can return its
    %% value.
    Value_S = case Value_S0 of
        undefined -> Other_Type_Mod:to_string(Value_C0);
        _         -> Value_S0
    end,
    try
        Value_C = Type_Mod:from_string(Value_S),
        Item1   = {Name, Value_S, Type_Mod, Value_C},
        get_loop2(Value_C, [Item1 | List_Head], List_Tail, true)
    catch
        throw:conversion_failed ->
            %% Item's value can't be converted. Return "undefined".
            get_loop2(undefined, [Item | List_Head], List_Tail,
              Converted)
    end;
get_loop(Name, Type_Mod, [Item | List_Tail], List_Head,
  Converted) ->
    get_loop(Name, Type_Mod, List_Tail, [Item | List_Head],
      Converted);
get_loop(_, _, [], Query, Converted) ->
    get_loop2(undefined, Query, [], Converted).

get_loop2(Value, _, _, false) ->
    Value;
get_loop2(Value, List_Head, List_Tail, true) ->
    List1 = lists:reverse(List_Head) ++ List_Tail,
    {Value, List1}.

set(State, Var, Value) ->
    set(State, Var, Value, undefined).

set(State, #var{prefix = undefined, name = "PATH"}, Value, undefined) ->
    State#state{
      path = Value
    };
set(State, #var{prefix = undefined, name = "SERVER_NAME"}, Value, undefined) ->
    State#state{
      server_name = Value
    };
set(State, #var{prefix = undefined, name = "METHOD"}, Value, undefined) ->
    State#state{
      method = Value
    };
set(State, #var{prefix = undefined, name = "SCHEME"}, Value, undefined) ->
    State#state{
      scheme = Value
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
set(State, #var{prefix = undefined, name = "URI"}, Value, undefined) ->
    case otis_utils:parse_uri(Value) of
        {Scheme, Host, Port, Path, Query} ->
            Host1 = otis_utils:format_host(Scheme, Host, Port),
            State1 = State#state{
              server_name  = Host,
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
set(State, #var{prefix = undefined, name = Name}, _, _) when
  Name == "RULE" orelse
  Name == "STATE" orelse
  Name == "REQUEST" orelse
  Name == "USER_VARS" ->
    ?WARNING("Tried to set the read-only variable \"~s\"~n", [Name]),
    State;
set(State, #var{name = [C | _] = Name} = Var, Value, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    Name1 = expand(State, Name),
    Var1  = Var#var{
      name = Name1
    },
    set(State, Var1, Value, Type_Mod);
set(State, #var{prefix = undefined, name = Name}, Value, Type_Mod) ->
    set_uservar(State, Name, Value, Type_Mod);
set(State, #var{prefix = "query", name = Name}, Value, Type_Mod) ->
    set_query(State, Name, Value, Type_Mod);
set(State, #var{prefix = "header", name = Name}, Value, Type_Mod) ->
    set_header(State, Name, Value, Type_Mod);
set(State, #var{prefix = "rheader", name = Name}, Value, Type_Mod) ->
    set_rheader(State, Name, Value, Type_Mod).

set_uservar(State, Name, Value) ->
    set_uservar(State, Name, Value, undefined).

set_uservar(State, [C | _] = Name, Value, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    Name1 = expand(State, Name),
    set_uservar(State, Name1, Value, Type_Mod);
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
    Name1 = expand(State, Name),
    set_query(State, Name1, Value, Type_Mod);
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
    Name1 = expand(State, Name),
    set_header(State, Name1, Value, Type_Mod);
set_header(#state{headers = Headers} = State, Name, Value, undefined) ->
    Name1 = string:to_lower(Name),
    Headers1 = case Value of
        "" ->
            unset_loop(Name1, Headers);
        _ ->
            Param = {Name1, Value, undefined, undefined},
            set_loop(Name1, Param, Headers)
    end,
    State#state{
      headers = Headers1
    };
set_header(#state{headers = Headers} = State, Name, Value, Type_Mod) ->
    Name1 = string:to_lower(Name),
    Param = {Name1, undefined, Type_Mod, Value},
    Headers1 = set_loop(Name1, Param, Headers),
    State#state{
      headers = Headers1
    }.

set_rheader(State, Name, Value) ->
    set_rheader(State, Name, Value, undefined).

set_rheader(State, [C | _] = Name, Value, Type_Mod)
  when not is_integer(C) ->
    %% The variable name is a variable itself: expand it.
    Name1 = expand(State, Name),
    set_rheader(State, Name1, Value, Type_Mod);
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
    List1;
set_loop2(_, _, [], List, true) ->
    List.

unset_loop(Name, List) ->
    unset_loop2(Name, List, []).

unset_loop2(Name, [{Name, _, _, _} | Rest], List) ->
    unset_loop2(Name, Rest, List);
unset_loop2(Name, [Item | Rest], List) ->
    unset_loop2(Name, Rest, [Item | List]);
unset_loop2(_, [], List) ->
    lists:reverse(List).
