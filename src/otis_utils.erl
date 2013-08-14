-module(otis_utils).

-include("otis.hrl").

%% Public API.
-export([
    type_mod/1,
    test_and_convert/2,
    expand_path/1,
    parse_uri/1,
    format_uri/1,
    unescape_uri/1,
    escape_uri/1,
    parse_host/1,
    format_host/3,
    server_ip/1,
    rebuild_path/1,
    split_query/1,
    parse_query/1,
    stringify_query/1,
    format_query/1,
    format_headers/1,
    capitalize_header/1,
    parse_cookies/1,
    parse_cookie_header/1,
    format_cookie_header/1,
    response_content/1,
    response_content/2
  ]).

%% Used by templates.
-export([
    return/1,
    stop/1,
    abort/1
  ]).

-define(IS_HEXA(C), (
    (C >= $a andalso C =< $f) orelse
    (C >= $A andalso C =< $F) orelse
    (C >= $0 andalso C =< $9)
  )).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

type_mod(int)  -> otis_type_int;
type_mod(port) -> otis_type_port;
type_mod(_)    -> undefined.

test_and_convert(Type_Mod, [C | _] = Value_S) when is_integer(C) ->
    case Type_Mod of
        undefined -> Value_S;
        _         -> Type_Mod:from_string(Value_S)
    end;
test_and_convert(_, Value_C) ->
    Value_C.

expand_path("$HOME") ->
    os:getenv("HOME");
expand_path("$HOME/" ++ Path) ->
    Home = os:getenv("HOME"),
    filename:join([Home, Path]);
expand_path(Path) ->
    Path.

%% -------------------------------------------------------------------
%% URI parsing.
%% -------------------------------------------------------------------

parse_uri([$/ | _] = String) ->
    parse_path(String, undefined, undefined, undefined);
parse_uri(String) ->
    parse_scheme(String, "").

parse_scheme([$:, $/, $/ | Rest], Scheme) ->
    Scheme1 = lists:reverse(Scheme),
    parse_host(Rest, Scheme1);
parse_scheme([C | Rest], Scheme) ->
    parse_scheme(Rest, [C | Scheme]);
parse_scheme([], _) ->
    undefined.

parse_host([$[ | Rest], Scheme) ->
    parse_host_rfc2732(Rest, "", Scheme);
parse_host(String, Scheme) ->
    parse_host2(String, "", Scheme).

parse_host2([C | _] = Rest, Host, Scheme)
  when C == $: orelse C == $/ orelse C == $? ->
    Host1 = lists:reverse(Host),
    parse_port(Rest, Scheme, Host1);
parse_host2([C | Rest], Host, Scheme) ->
    parse_host2(Rest, [C | Host], Scheme);
parse_host2([], Host, Scheme) ->
    Host1 = lists:reverse(Host),
    {Scheme, Host1, default_port(Scheme), "/", ""}.

parse_host_rfc2732([$] | Rest], Host, Scheme) ->
    Host1 = lists:reverse(Host),
    parse_port(Rest, Scheme, Host1);
parse_host_rfc2732([C | Rest], Host, Scheme) ->
    parse_host_rfc2732(Rest, [C | Host], Scheme);
parse_host_rfc2732([], _, _) ->
    undefined.

parse_port([$: | Rest], Scheme, Host) ->
    parse_port2(Rest, 0, Scheme, Host);
parse_port(String, Scheme, Host) ->
    parse_path(String, Scheme, Host, default_port(Scheme)).

parse_port2([C | Rest], Port, Scheme, Host) when C >= $0 andalso C =< $9 ->
    C1 = C - $0,
    parse_port2(Rest, Port * 10 + C1, Scheme, Host);
parse_port2([C | _] = Rest, Port, Scheme, Host) when C == $/ orelse C == $? ->
    parse_path(Rest, Scheme, Host, Port);
parse_port2([], Port, Scheme, Host) ->
    {Scheme, Host, Port, "/", ""};
parse_port2(_, _, _, _) ->
    undefined.

parse_path([$/ | Rest], Scheme, Host, Port) ->
    parse_path2(Rest, "/", Scheme, Host, Port);
parse_path([$? | _] = Rest, Scheme, Host, Port) ->
    parse_querystr(Rest, Scheme, Host, Port, "/");
parse_path([], Scheme, Host, Port) ->
    {Scheme, Host, Port, "/", ""}.

parse_path2([$? | _] = Rest, Path, Scheme, Host, Port) ->
    Path1 = unescape_uri(lists:reverse(Path)),
    parse_querystr(Rest, Scheme, Host, Port, Path1);
parse_path2([C | Rest], Path, Scheme, Host, Port) ->
    parse_path2(Rest, [C | Path], Scheme, Host, Port);
parse_path2([], Path, Scheme, Host, Port) ->
    Path1 = unescape_uri(lists:reverse(Path)),
    {Scheme, Host, Port, Path1, ""}.

parse_querystr([$? | Rest], Scheme, Host, Port, Path) ->
    {Scheme, Host, Port, Path, Rest};
parse_querystr(Rest, Scheme, Host, Port, Path) ->
    {Scheme, Host, Port, Path, Rest}.

default_port("http")  ->  80;
default_port("https") -> 443;
default_port(_)       ->   0.

format_uri(#state{scheme = Scheme} = State) ->
    {State1, Host} = case otis_var:get_header(State, "host") of
        {S1, undefined} -> {S1, State#state.host};
        {S1, H1}        -> {S1, H1}
    end,
    {State2, Path} = rebuild_path(State1),
    URI = Scheme ++ "://" ++ Host ++ Path,
    {State2, URI}.

unescape_uri(String) ->
    unescape_uri(String, "").

unescape_uri([$+ | Rest], Result) ->
    C = $\s,
    unescape_uri(Rest, [C | Result]);
unescape_uri([$%, C1, C2  | Rest], Result)
  when ?IS_HEXA(C1) andalso ?IS_HEXA(C2) ->
    C1p = hexa_to_decimal(C1),
    C2p = hexa_to_decimal(C2),
    C = C1p * 16 + C2p,
    unescape_uri(Rest, [C | Result]);
unescape_uri([C | Rest], Result) ->
    unescape_uri(Rest, [C | Result]);
unescape_uri([], Result) ->
    lists:reverse(Result).

hexa_to_decimal(C) when (C >= $a andalso C =< $f) -> 10 + C - $a;
hexa_to_decimal(C) when (C >= $A andalso C =< $F) -> 10 + C - $A;
hexa_to_decimal(C) when (C >= $0 andalso C =< $9) -> C - $0.

escape_uri(String) ->
    escape_uri(String, "").

escape_uri([C | Rest], Result) when
  (C >= $a andalso C =< $z) orelse
  (C >= $A andalso C =< $Z) orelse
  (C >= $0 andalso C =< $9) orelse
  C == $- orelse C == $/ orelse C == $: orelse C == $_ orelse
  C == $@ orelse C == $. orelse C == $, orelse C == $! ->
    escape_uri(Rest, Result ++ [C]);
escape_uri([C | Rest], Result) ->
    C1 = lists:flatten(io_lib:format("%~.16B", [C])),
    escape_uri(Rest, Result ++ C1);
escape_uri([], Result) ->
    Result.

parse_host(undefined) ->
    undefined;
parse_host(Host) ->
    case parse_host(Host, "") of
        {_, Host1, Port, _, _} -> {Host1, Port};
        _                      -> undefined
    end.

format_host(Scheme, Host, Port) ->
    Host1 = case string:chr(Host, $:) of
        0 -> Host;
        _ -> "[" ++ Host ++ "]"
    end,
    case {Scheme, Port} of
        {"http",   80} -> Host1;
        {"https", 443} -> Host1;
        _              -> Host1 ++ ":" ++ integer_to_list(Port)
    end.

server_ip(Host) ->
    case inet_parse:address(Host) of
        {ok, Addr} ->
            Addr;
        _ ->
            case inet:getaddr(Host, inet6) of
                {ok, Addr} ->
                    Addr;
                _ ->
                    case inet:getaddr(Host, inet) of
                        {ok, Addr} -> Addr;
                        _          -> undefined
                    end
            end
    end.

%% -------------------------------------------------------------------
%% Path handling.
%% -------------------------------------------------------------------

rebuild_path(#state{query_parsed = true} = State) ->
    State1 = stringify_query(State),
    rebuild_path(State1);
rebuild_path(#state{path = Path, query_str = ""} = State) ->
    rebuild_path2(State, escape_uri(Path));
rebuild_path(#state{path = Path, query_str = Query} = State) ->
    rebuild_path2(State, escape_uri(Path) ++ "?" ++ Query).

rebuild_path2(#state{fragment = ""} = State, Path) ->
    {State, Path};
rebuild_path2(#state{fragment = Fragment} = State, Path) ->
    {State, Path ++ "#" ++ Fragment}.

%% -------------------------------------------------------------------
%% Query string handling.
%% -------------------------------------------------------------------

split_query(Path) ->
    {_, _, _, Path1, Query} = parse_path(Path, "", "", 0),
    {Path1, Query}.

parse_query(#state{query_parsed = true} = State) ->
    State;
parse_query(#state{query_str = Query} = State) ->
    Params = string:tokens(Query, "&"),
    Query1 = [
      case string:chr(P, $=) of
          0 ->
              {
                unescape_uri(P), "",
                undefined, undefined
              };
          I ->
              {
                unescape_uri(string:substr(P, 1, I - 1)),
                unescape_uri(string:substr(P, I + 1)),
                undefined, undefined
              }
      end
      || P <- Params
    ],
    State#state{
      query_str    = Query1,
      query_parsed = true
    }.

stringify_query(#state{query_parsed = false} = State) ->
    State;
stringify_query(#state{query_str = Query} = State) ->
    Query1 = format_query(Query),
    State#state{
      query_str    = Query1,
      query_parsed = false
    }.

format_query(Query) ->
    format_query2(Query, []).

format_query2([{Name, Value, _, _} | Rest], Result) when Value /= undefined ->
    Param = format_param(Name, Value),
    format_query2(Rest, [Param | Result]);
format_query2([{Name, _, Type_Mod, Value_C} | Rest], Result) ->
    Value_S = Type_Mod:to_string(Value_C),
    Param = format_param(Name, Value_S),
    format_query2(Rest, [Param | Result]);
format_query2([], Result) ->
    string:join(lists:reverse(Result), "&").

format_param(Name, Value) ->
    Name1 = escape_uri(Name, ""),
    Value1 = escape_uri(Value, ""),
    Name1 ++ "=" ++ Value1.

%% -------------------------------------------------------------------
%% Headers handling.
%% -------------------------------------------------------------------

format_headers(Headers) ->
    format_headers2(Headers, "").

format_headers2([{Name, Value, _, _} | Rest], Result) when Value /= undefined ->
    Header = format_header(Name, Value),
    format_headers2(Rest, [Header | Result]);
format_headers2([{Name, undefined, Type_Mod, Value_C} | Rest], Result) ->
    Value_S = Type_Mod:to_string(Value_C),
    Header  = format_header(Name, Value_S),
    format_headers2(Rest, [Header | Result]);
format_headers2([], Result) ->
    string:join(lists:reverse(Result), "").

format_header(Name, Value) ->
    Name1 = capitalize_header(Name),
    io_lib:format("~s: ~s\r\n", [Name1, Value]).

capitalize_header(Name) ->
    capitalize_header2(Name, "").

capitalize_header2([C | Rest], "") when C >= $a andalso C =< $z ->
    capitalize_header2(Rest, [C - $a + $A]);
capitalize_header2([$-, C | Rest], Result) when C >= $a andalso C =< $z ->
    capitalize_header2(Rest, [C - $a + $A, $- | Result]);
capitalize_header2([C | Rest], Result) ->
    capitalize_header2(Rest, [C | Result]);
capitalize_header2([], Result) ->
    lists:reverse(Result).

%% -------------------------------------------------------------------
%% Cookie handling.
%% -------------------------------------------------------------------

parse_cookies(State) ->
    otis_var:headers_to_cookies(State).

parse_cookie_header(String) ->
    parse_cookie_header2(String, "", undefined, []).

parse_cookie_header2([$\s | Rest], "", undefined, Result) ->
    parse_cookie_header2(Rest, "", undefined, Result);
parse_cookie_header2([$= | Rest], Name, undefined, Result) ->
    parse_cookie_header2(Rest, Name, "", Result);
parse_cookie_header2([$; | Rest], Name, Value, Result) ->
    Result1 = store_cookie(Result, Name, Value),
    parse_cookie_header2(Rest, "", undefined, Result1);
parse_cookie_header2([C | Rest], Name, undefined, Result) ->
    parse_cookie_header2(Rest, [C | Name], undefined, Result);
parse_cookie_header2([C | Rest], Name, Value, Result) ->
    parse_cookie_header2(Rest, Name, [C | Value], Result);
parse_cookie_header2([], Name, Value, Result) ->
    Result1 = store_cookie(Result, Name, Value),
    lists:reverse(Result1).

store_cookie(Result, "", _) ->
    Result;
store_cookie(Result, Name, Value) ->
    Name1 = unescape_uri(lists:reverse(Name)),
    Value1 = case Value of
        undefined -> "";
        _         -> unescape_uri(lists:reverse(Value))
    end,
    [{Name1, Value1, undefined, undefined} | Result].

format_cookie_header(Cookies) ->
    format_cookie_header2(Cookies, undefined).

format_cookie_header2([{Name, Value, _, _} | Rest], Result)
  when Value /= undefined ->
    Cookie  = format_cookie(Name, Value),
    Result1 = case Result of
        undefined -> Cookie;
        _         -> Result ++ "; " ++ Cookie
    end,
    format_cookie_header2(Rest, Result1);
format_cookie_header2([{Name, undefined, Type_Mod, Value_C} | Rest],
  Result) ->
    Value_S = Type_Mod:to_string(Value_C),
    Cookie  = format_cookie(Name, Value_S),
    Result1 = case Result of
        undefined -> Cookie;
        _         -> Result ++ "; " ++ Cookie
    end,
    format_cookie_header2(Rest, Result1);
format_cookie_header2([], Result) ->
    Result.

format_cookie(Name, Value) ->
    Name1  = escape_uri(Name),
    Value1 = escape_uri(Value),
    Name1 ++ "=" ++ Value1.

%% -------------------------------------------------------------------
%% HTTP errors.
%% -------------------------------------------------------------------

response_content(Code) ->
    response_content(Code, httpd_util:reason_phrase(Code)).

response_content(Code, _)
  when Code >= 301 andalso Code =< 307 andalso Code /= 304 ->
    {undefined, <<>>};
response_content(Code, Reason) ->
    Content = list_to_binary(io_lib:format(
        "<html>"
        "<head><title>~b ~s</title></head>"
        "<body><h1>~b ~s</h1></body>"
        "</html>~n",
        [Code, Reason, Code, Reason])),
    {"text/html; charset=UTF-8", Content}.

%% -------------------------------------------------------------------
%% Used by templates.
%% -------------------------------------------------------------------

return(State) ->
    %% This is called by the last rule when it succeeds.
    State.

stop(State) ->
    %% This is called by any rule who stops.
    throw({stop, State}).

abort(State) ->
    %% This is called by any rule who fails.
    throw({abort, State}).
