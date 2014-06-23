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

-module(otis_app).

-behaviour(application).

%% Configuration API.
-export([
    params_list/0,
    get_param/1,
    is_param_valid/2,
    set_param/2,
    check_and_set_param/2,
    show_params/0,
    check_params/0,
    log_param_errors/1
  ]).

%% Developments utils.
-export([
    from_builddir/0
  ]).

%% application(3erl) callbacks.
-export([
    start/2,
    stop/1,
    config_change/3
  ]).

%% -------------------------------------------------------------------
%% Configuration API.
%% -------------------------------------------------------------------

-spec params_list() -> [atom()].

params_list() ->
    [
      config,
      templates_dir,
      engine_save_dir,
      geoip_db
    ].

-spec get_param(atom()) -> term().

get_param(Param) ->
    {ok, Value} = application:get_env(otis, Param),
    Value.

-spec is_param_valid(atom(), term()) -> boolean().

is_param_valid(_Param, '$MANDATORY') ->
    false;
is_param_valid(config, none) ->
    true;
is_param_valid(config, Value) when is_list(Value) ->
    filelib:is_regular(Value);
is_param_valid(templates_dir, default) ->
    true;
is_param_valid(templates_dir, Value) ->
    Path = otis_utils:expand_path(Value),
    filelib:is_dir(Path);
is_param_valid(engine_save_dir, "") ->
    true;
is_param_valid(engine_save_dir, Value) ->
    Path = otis_utils:expand_path(Value),
    filelib:is_dir(Path);
is_param_valid(geoip_db, none) ->
    true;
is_param_valid(geoip_db, Path) ->
    io_lib:char_list(Path);
is_param_valid(_Param, _Value) ->
    false.

-spec set_param(atom(), term()) -> ok.

set_param(Param, Value) ->
    application:set_env(otis, Param, Value).

-spec check_and_set_param(atom(), term()) -> ok.

check_and_set_param(Param, Value) ->
    %% If the value is invalid, this function logs an error through
    %% error_logger:warning_msg/2 but always returns 'ok'. To check a
    %% value programmatically, use the is_param_valid/2 function.
    case is_param_valid(Param, Value) of
        true  -> set_param(Param, Value);
        false -> log_param_errors([Param])
    end.

-spec show_params() -> ok.

show_params() ->
    Fun = fun(Param) ->
        Value = get_param(Param),
        io:format("~s: ~p~n", [Param, Value])
    end,
    lists:foreach(Fun, params_list()).

-spec check_params() -> boolean().

check_params() ->
    Fun = fun(Param) ->
        Value = get_param(Param),
        not is_param_valid(Param, Value)
    end,
    Bad_Params = lists:filter(Fun, params_list()),
    case Bad_Params of
        [] ->
            true;
        _ ->
            log_param_errors(Bad_Params),
            false
    end.

-spec log_param_errors([atom()]) -> ok.

log_param_errors([]) ->
    ok;
log_param_errors([config = Param | Rest]) ->
    error_logger:warning_msg(
      "otis: invalid value for \"~s\": ~p.~n"
      "It must be a filename or the atom \"none\".~n",
      [Param, get_param(Param)]),
    log_param_errors(Rest);
log_param_errors([templates_dir = Param | Rest]) ->
    error_logger:warning_msg(
      "otis: invalid value for \"~s\": ~p.~n"
      "It must be a dirname or the atom \"default\".~n",
      [Param, get_param(Param)]),
    log_param_errors(Rest);
log_param_errors([engine_save_dir = Param | Rest]) ->
    error_logger:warning_msg(
      "otis: invalid value for \"~s\": ~p.~n"
      "It must be a dirname.~n",
      [Param, get_param(Param)]),
    log_param_errors(Rest);
log_param_errors([geoip_db = Param | Rest]) ->
    error_logger:warning_msg(
      "otis: invalid value for \"~s\": ~p.~n"
      "It must be a 'none' or a path to GeoIP2-City.mmdb.~n",
      [Param, get_param(Param)]),
    log_param_errors(Rest);
log_param_errors([Param | Rest]) ->
    error_logger:warning_msg(
      "otis: unknown parameter \"~s\".~n",
      [Param]),
    log_param_errors(Rest).

%% -------------------------------------------------------------------
%% Development utils.
%% -------------------------------------------------------------------

from_builddir() ->
    Dir = filename:dirname(code:which(?MODULE)),
    Makefile = filename:join([Dir, "Makefile"]),
    filelib:is_file(Makefile).

%% -------------------------------------------------------------------
%% application(3erl) callbacks.
%% -------------------------------------------------------------------

-spec start(normal | {takeover, atom()} | {failover, atom()}, term()) ->
    {ok, pid()} | {error, term()}.

start(_, _) ->
    Steps = [
      check_params,
      add_save_dir_to_code_path,
      load_engine
    ],
    case do_start(Steps) of
        {error, Reason, Message} ->
            Log = case application:get_env(kernel, error_logger) of
                {ok, {file, File}} -> "Check log file \"" ++ File ++ "\".";
                {ok, tty}          -> "Check standard output.";
                _                  -> "No log configured..."
            end,
            %% The following message won't be visible if Erlang was
            %% detached from the terminal.
            io:format(standard_error, "ERROR: ~s~s~n~n", [Message, Log]),
            error_logger:error_msg(Message),
            {error, Reason};
        Ret ->
            Ret
    end.

-spec do_start([term()]) ->
    {ok, pid()} |
    ignore      |
    {error, {already_started, pid()} | shutdown | term()} |
    {error, atom(), term()}.

do_start([check_params | Rest]) ->
    case check_params() of
        true ->
            do_start(Rest);
        false ->
            Message = io_lib:format(
              "otis: invalid application configuration~n", []),
            {error, invalid_configuration, Message}
    end;
do_start([add_save_dir_to_code_path | Rest]) ->
    %% Add the engine save directory to the code path, because the
    %% engine source file and beam file are written to this directory.
    %% We add it to the end of the paths list because this directory is
    %% writable by the node: we don't want to allow to override other
    %% modules.
    code:add_pathz(get_param(engine_save_dir)),
    do_start(Rest);
do_start([load_engine | Rest]) ->
    %% If the user specified a configuration file, load the engine.
    case get_param(config) of
        none ->
            do_start(Rest);
        _ ->
            try
                otis:reload_engine(),
                do_start(Rest)
            catch
                _:Exception ->
                    Message = io_lib:format(
                      "otis: failed to load rewrite rules: ~p~n",
                      [Exception]),
                    {error, rules_load_failure, Message}
            end
    end;
do_start([]) ->
    otis_sup:start_link().

-spec stop(term()) -> ok.

stop(_) ->
    ok.

-spec config_change([{atom(), term()}], [{atom(), term()}], [atom()]) -> ok.

config_change(_, _, _) ->
    ok.
