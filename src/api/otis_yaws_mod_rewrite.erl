-module(otis_yaws_mod_rewrite).

-include_lib("yaws/include/yaws_api.hrl").

-include("otis.hrl").

%% Public API.
-export([
    arg_rewrite/1
  ]).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

arg_rewrite(#arg{clisock = Socket, req = Req, headers = Headers0} = ARG) ->
    %% Called by Yaws. We must convert the Yaws' structure to the
    %% internal one.
    %% Client and server IP address/port.
    {Client_IP, Client_Port} = case ARG#arg.client_ip_port of
        {unknown, _} -> {undefined, undefined};
        CIP          -> CIP
    end,
    {Sock_Mod, Scheme} = if
        is_port(Socket) -> {inet, "http"};
        true            -> {ssl, "https"}
    end,
    {Server_IP, Server_Port} = case Sock_Mod:sockname(Socket) of
        {ok, SIP} -> SIP;
        _         -> {undefined, undefined}
    end,
    {Server_Name, _} = otis_utils:parse_host(Headers0#headers.host),
    Method0 = Req#http_request.method,
    Method  = if
        is_atom(Method0) -> atom_to_list(Method0);
        true             -> Method0
    end,
    try
        Path0 = case Req#http_request.path of
            {abs_path, P0} -> P0;
            _              -> throw(invalid_request)
        end,
        Headers = headers_from_yaws(Headers0),
        %% Path and query string. This code is in the try/catch if URI
        %% parsing fails.
        {Path, Query}  = otis_utils:split_query(Path0),
        %% We can now build our internal state and proceed with rules
        %% evaluation.
        State = #state{
          client_ip    = Client_IP,
          client_port  = Client_Port,
          server_ip    = Server_IP,
          server_port  = Server_Port,
          server_name  = Server_Name,
          method       = Method,
          scheme       = Scheme,
          path         = Path,
          query_str    = Query,
          query_parsed = false,
          headers      = Headers
        },
        State1 = otis_reqrw_engine:eval(State),
        %% We need to convert our internal state back to the Yaws'
        %% structure.
        back_to_yaws(State1, ARG)
    catch
        throw:invalid_request ->
            %% This happends when:
            %%   1. The request has no path and Yaws puts the HTTP version
            %%      in the path.
            %%   2. Yaws puts http_error returned from decode_packet in
            %%      "others".
            %% We answer it with a 400 Bad Request.
            ?ERROR("Invalid HTTP request: ~p~n",
              [ARG#arg{clidata = body_skipped}]),
            {Content_Type, Content} = otis_utils:response_content(400),
            RHeaders = rheaders_to_yaws([
                {"content-type", Content_Type, undefined, undefined}
              ]),
            Resp = #rewrite_response{
              status  = 400,
              headers = RHeaders,
              content = Content
            },
            ARG#arg{
              state = Resp
            };
        error:{badmatch, _} ->
            %% otis_utils:split_query/1 returned an error. Respond
            %% with a 500 code.
            ?ERROR("Failed to parse URI: ~p", [Req#http_request.path]),
            {Content_Type, Content} = otis_utils:response_content(500),
            RHeaders = rheaders_to_yaws([
                {"content-type", Content_Type, undefined, undefined}
              ]),
            Resp = #rewrite_response{
              status  = 500,
              headers = RHeaders,
              content = Content
            },
            ARG#arg{
              state = Resp
            };
        error:undef ->
            %% The otis_reqrw_engine generated module isn't available yet!
            ?ERROR("The \"otis_reqrw_engine\" request rewriting engine "
              "isn't available!~n", []),
            {Content_Type, Content} = otis_utils:response_content(500),
            RHeaders = rheaders_to_yaws([
                {"content-type", Content_Type, undefined, undefined}
              ]),
            Resp = #rewrite_response{
              status  = 500,
              headers = RHeaders,
              content = Content
            },
            ARG#arg{
              state = Resp
            };
        _:Exception ->
            %% Return a 500 code.
            ?ERROR("Request rewriting engine crash:~n~p~n~p~n", [
                Exception,
                erlang:get_stacktrace()
              ]),
            {Content_Type, Content} = otis_utils:response_content(500),
            RHeaders = rheaders_to_yaws([
                {"content-type", Content_Type, undefined, undefined}
              ]),
            Resp = #rewrite_response{
              status  = 500,
              headers = RHeaders,
              content = Content
            },
            ARG#arg{
              state = Resp
            }
    end.

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

back_to_yaws(
  #state{response = false, method = Method0, headers = Headers} = State,
  #arg{req = Req} = ARG) ->
    %% Update HTTP request structure.
    Method = case Method0 of
        "DELETE"  -> 'DELETE';
        "GET"     -> 'GET';
        "HEAD"    -> 'HEAD';
        "OPTIONS" -> 'OPTIONS';
        "POST"    -> 'POST';
        "PUT"     -> 'PUT';
        _         -> Method0
    end,
    {_, Path} = otis_utils:rebuild_path(State),
    Req1 = Req#http_request{
      method = Method,
      path   = {abs_path, Path}
    },
    %% Rebuild headers structure.
    Headers1 = headers_to_yaws(Headers),
    ARG#arg{
      req     = Req1,
      headers = Headers1
    };
back_to_yaws(
  #state{response = true, code = Code, reason = Reason, rheaders = Headers},
  ARG) ->
    Headers1 = rheaders_to_yaws(Headers),
    Content  = otis_utils:response_content(Code, Reason),
    Resp = #rewrite_response{
      status  = Code,
      headers = Headers1,
      content = Content
    },
    ARG#arg{
      state = Resp
    }.

headers_from_yaws(Yaws) ->
    headers_from_yaws2(Yaws, record_info(fields, headers), []).

headers_from_yaws2(#headers{connection = Value} = Yaws,
  [connection | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"connection", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{accept = Value} = Yaws,
  [accept | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"accept", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{host = Value} = Yaws,
  [host | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"host", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{if_modified_since = Value} = Yaws,
  [if_modified_since | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"if-modified-since", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{if_match = Value} = Yaws,
  [if_match | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"if-match", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{if_none_match = Value} = Yaws,
  [if_none_match | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"if-none-match", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{if_range = Value} = Yaws,
  [if_range | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"if-range", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{if_unmodified_since = Value} = Yaws,
  [if_unmodified_since | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"if-unmodified-since", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{range = Value} = Yaws,
  [range | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"range", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{referer = Value} = Yaws,
  [referer | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"referer", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{user_agent = Value} = Yaws,
  [user_agent | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"user-agent", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{accept_ranges = Value} = Yaws,
  [accept_ranges | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"accept-ranges", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{cookie = List} = Yaws,
  [cookie | Rest], Headers) when List /= [] ->
    Cookies = [
      {"cookie", V, undefined, undefined}
      || V <- List
    ],
    Headers1 = Cookies ++ Headers,
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{keep_alive = Value} = Yaws,
  [keep_alive | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"keep-alive", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{location = Value} = Yaws,
  [location | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"location", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{content_length = Value} = Yaws,
  [content_length | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"content-length", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{content_type = Value} = Yaws,
  [content_type | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"content-type", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{authorization = {_, _, Orig}} = Yaws,
  [authorization | Rest], Headers) ->
    Headers1 = [
      {"authorization", Orig, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{transfer_encoding = Value} = Yaws,
  [transfer_encoding | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"transfer-encoding", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{x_forwarded_for = Value} = Yaws,
  [x_forwarded_for | Rest], Headers) when Value /= undefined ->
    Headers1 = [
      {"x-forwarded-for", Value, undefined, undefined}
      | Headers],
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(#headers{other = Yaws_Hds} = Yaws,
  [other | Rest], Headers) when Yaws_Hds /= undefined andalso Yaws_Hds /= [] ->
    Fun = fun
        ({http_header, _, Name0, _, Value0}) ->
            Name1 = if
                is_atom(Name0) -> atom_to_list(Name0);
                true           -> Name0
            end,
            Name  = string:to_lower(Name1),
            Value = if
                is_binary(Value0) -> binary_to_list(Value0);
                true              -> Value0
            end,
            {Name, Value, undefined, undefined};
        ({http_error, _}) ->
            throw(invalid_request)
    end,
    Headers1 = Headers ++ lists:map(Fun, Yaws_Hds),
    headers_from_yaws2(Yaws, Rest, Headers1);
headers_from_yaws2(Yaws, [_ | Rest], Headers) ->
    headers_from_yaws2(Yaws, Rest, Headers);
headers_from_yaws2(_, [], Headers) ->
    Headers.

headers_to_yaws(Headers) ->
    headers_to_yaws2(Headers, #headers{}).

headers_to_yaws2([{Name, undefined, Type_Mod, Value_C} | Rest], Headers) ->
    Value_S = Type_Mod:to_string(Value_C),
    headers_to_yaws2([{Name, Value_S, Type_Mod, Value_C} | Rest], Headers);
headers_to_yaws2([{"connection", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      connection = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"accept", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      accept = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"host", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      host = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"if-modified-since", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      if_modified_since = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"if-match", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      if_match = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"if-none-match", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      if_none_match = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"if-range", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      if_range = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"if-unmodified-since", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      if_unmodified_since = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"range", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      range = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"referer", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      referer = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"user-agent", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      user_agent = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"accept-ranges", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      accept_ranges = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"cookie", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      cookie = [Value | Headers#headers.cookie]
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"keep-alive", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      keep_alive = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"location", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      location = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"content-length", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      content_length = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"content-type", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      content_type = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"authorization", Value, _, _} | Rest], Headers) ->
    Auth = yaws:parse_auth(Value),
    Headers1 = Headers#headers{
      authorization = Auth
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"transfer-encoding", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      transfer_encoding = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{"x-forwarded-for", Value, _, _} | Rest], Headers) ->
    Headers1 = Headers#headers{
      x_forwarded_for = Value
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([{Name, Value, _, _} | Rest],
  #headers{other = Other} = Headers) ->
    Headers1 = Headers#headers{
      other = [{http_header, 0, erlang_header_name(Name), undefined, Value}
        | Other]
    },
    headers_to_yaws2(Rest, Headers1);
headers_to_yaws2([], Headers) ->
    Headers.

erlang_header_name("cache-control")       -> 'Cache-Control';
erlang_header_name("date")                -> 'Date';
erlang_header_name("pragma")              -> 'Pragma';
erlang_header_name("upgrade")             -> 'Upgrade';
erlang_header_name("via")                 -> 'Via';
erlang_header_name("accept-charset")      -> 'Accept-Charset';
erlang_header_name("accept-encoding")     -> 'Accept-Encoding';
erlang_header_name("accept-language")     -> 'Accept-Language';
erlang_header_name("from")                -> 'From';
erlang_header_name("max-forwards")        -> 'Max-Forwards';
erlang_header_name("proxy-authorization") -> 'Proxy-Authorization';
erlang_header_name("age")                 -> 'Age';
erlang_header_name("proxy-authenticate")  -> 'Proxy-Authenticate';
erlang_header_name("public")              -> 'Public';
erlang_header_name("retry-after")         -> 'Retry-After';
erlang_header_name("server")              -> 'Server';
erlang_header_name("vary")                -> 'Vary';
erlang_header_name("warning")             -> 'Warning';
erlang_header_name("www-authenticate")    -> 'Www-Authenticate';
erlang_header_name("allow")               -> 'Allow';
erlang_header_name("content-base")        -> 'Content-Base';
erlang_header_name("content-encoding")    -> 'Content-Encoding';
erlang_header_name("content-language")    -> 'Content-Language';
erlang_header_name("content-location")    -> 'Content-Location';
erlang_header_name("content-md5")         -> 'Content-Md5';
erlang_header_name("content-range")       -> 'Content-Range';
erlang_header_name("etag")                -> 'Etag';
erlang_header_name("expires")             -> 'Expires';
erlang_header_name("last-modified")       -> 'Last-Modified';
erlang_header_name("set-cookie")          -> 'Set-Cookie';
erlang_header_name("set-cookie2")         -> 'Set-Cookie2';
erlang_header_name("proxy-connection")    -> 'Proxy-Connection';
erlang_header_name(Name)                  -> otis_utils:capitalize_header(Name).

rheaders_to_yaws(Headers) ->
    rheaders_to_yaws2(Headers, []).

rheaders_to_yaws2([{Name, undefined, Type_Mod, Value_C} | Rest], Headers) ->
    Value_S = Type_Mod:to_string(Value_C),
    rheaders_to_yaws2([{Name, Value_S, Type_Mod, Value_C} | Rest], Headers);
rheaders_to_yaws2([{"connection", Value, _, _} | Rest], Result) ->
    Header = {connection, Value},
    rheaders_to_yaws2(Rest, [{header, Header} | Result]);
rheaders_to_yaws2([{"location", Value, _, _} | Rest], Result) ->
    Header = {location, Value},
    rheaders_to_yaws2(Rest, [{header, Header} | Result]);
rheaders_to_yaws2([{"cache-control", Value, _, _} | Rest], Result) ->
    Header = {cache_control, Value},
    rheaders_to_yaws2(Rest, [{header, Header} | Result]);
rheaders_to_yaws2([{"set_cookie", Value, _, _} | Rest], Result) ->
    Header = {set_cookie, Value},
    rheaders_to_yaws2(Rest, [{header, Header} | Result]);
rheaders_to_yaws2([{"content-type", Value, _, _} | Rest], Result) ->
    Header = {content_type, Value},
    rheaders_to_yaws2(Rest, [{header, Header} | Result]);
rheaders_to_yaws2([{"content-length", Value, _, _} | Rest], Result) ->
    Header = {content_length, Value},
    rheaders_to_yaws2(Rest, [{header, Header} | Result]);
rheaders_to_yaws2([{"content-encoding", Value, _, _} | Rest], Result) ->
    Header = {content_encoding, Value},
    rheaders_to_yaws2(Rest, [{header, Header} | Result]);
rheaders_to_yaws2([{Name, Value, _, _} | Rest], Result) ->
    Name1  = otis_utils:capitalize_header(Name),
    Header = {Name1, Value},
    rheaders_to_yaws2(Rest, [{header, Header} | Result]);
rheaders_to_yaws2([], Result) ->
    Result.
