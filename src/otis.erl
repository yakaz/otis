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

-module(otis).

-include_lib("kernel/include/file.hrl").

-include("otis.hrl").

%% Public API.
-export([
    reload_engine/0,
    reload_engine/1,
    reload_engine_if_modified/0,
    reload_engine_if_modified/1,
    remove_saved_engine/0,
    cover_compile_engine/1,
    cover_compile_engine_if_modified/1
  ]).

-define(ENGINE_FILE, "otis_reqrw_engine.erl").
-define(ENGINE_MOD,  otis_reqrw_engine).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

reload_engine() ->
    case otis_conf:load() of
        undefined -> ok;
        Ruleset   -> gen_engine(Ruleset)
    end.

reload_engine(File) ->
    Ruleset = otis_conf:load(File),
    gen_engine(Ruleset).

reload_engine_if_modified() ->
    case otis_app:get_param(config) of
        none -> ok;
        File -> reload_engine_if_modified(File)
    end.

reload_engine_if_modified(File) ->
    try
        case ?ENGINE_MOD:conf() of
            File ->
                %% An engine is already loaded and was generated from
                %% the given configuration file. We must compare the
                %% modification time of the configuration file and the
                %% compiled module.
                {ok, #file_info{mtime = Time1}} = file:read_file_info(File),
                {ok, #file_info{mtime = Time2}} = file:read_file_info(
                  engine_source_file()),
                if
                    Time1 =< Time2 ->
                        %% The configuration file was not modified.
                        %% Don't touch anything.
                        ?INFO("Engine loaded from save directory~n", []),
                        ok;
                    true ->
                        %% The configuration file is newer: reload the
                        %% engine.
                        reload_engine(File)
                end;
            _ ->
                %% The engine was generated from another configuration
                %% file. We must reload it.
                reload_engine(File)
        end
    catch
        error:undef ->
            %% The engine isn't generated yet.
            reload_engine(File);
        error:{badmatch, {error, _}} ->
            %% The engine is loaded but the beam file doesn't exist:
            %% reload it.
            reload_engine(File)
    end.

remove_saved_engine() ->
    Source = engine_source_file(),
    Beam   = filename:rootname(Source) ++ ".beam",
    case file:delete(Source) of
        ok ->
            ?INFO("Engine source file removed~n", []),
            case file:delete(Beam) of
                ok ->
                    ?INFO("Engine beam file removed~n", []);
                {error, enoent} ->
                    ?INFO("Engine beam file not present~n", []);
                {error, Reason} = Ret ->
                    ?ERROR("Failed to remove engine beam file \"~s\": ~p~n",
                      [Beam, Reason]),
                    Ret
            end;
        {error, enoent} ->
            ?INFO("Engine source file not present~n", []);
        {error, Reason} = Ret ->
            ?ERROR("Failed to remove engine source file \"~s\": ~p~n",
              [Source, Reason]),
            Ret
    end.

cover_compile_engine(Keep_Data) ->
    %% Cover-compile the engine. But first, if the caller asked for it,
    %% export any existing data, because the build will flush them.
    MI = ?ENGINE_MOD:module_info(),
    Compile = proplists:get_value(compile, MI),
    Source  = proplists:get_value(source, Compile),
    Options = proplists:get_value(options, Compile),
    Export  = filename:rootname(Source) ++ ".coverdata",
    Exported = case cover:is_compiled(?ENGINE_MOD) of
        {file, _} when Keep_Data ->
            cover:export(Export, ?ENGINE_MOD),
            true;
        _ ->
            false
    end,
    case cover:compile_module(?ENGINE_MOD, Options) of
        {ok, _} ->
            ?INFO("Engine cover-compiled~n", []);
        {error, Reason} ->
            ?ERROR("Failed to cover-compile engine: ~p~n", [Reason])
    end,
    if
        Exported ->
            cover:import(Export),
            file:delete(Export);
        true ->
            ok
    end.

cover_compile_engine_if_modified(Keep_Data) ->
    case lists:member(?ENGINE_MOD, cover:modules()) of
        true  -> ?INFO("Engine already cover-compiled~n", []);
        false -> cover_compile_engine(Keep_Data)
    end.

%% -------------------------------------------------------------------
%% Low-level engine handling.
%% -------------------------------------------------------------------

engine_source_file() ->
    Dir = otis_app:get_param(engine_save_dir),
    case Dir of
        "" -> ?ENGINE_FILE;
        _  -> filename:join([otis_utils:expand_path(Dir), ?ENGINE_FILE])
    end.

gen_engine(Ruleset) ->
    Code = otis_tpl:gen(Ruleset),
    File = engine_source_file(),
    case file:open(File, [write, {encoding, unicode}]) of
        {ok, FD} ->
            file:write(FD, Code),
            file:close(FD),
            compile_engine(File);
        {error, Reason} ->
            ?ERROR("Failed to save engine "
              "to \"~s\": ~s", [File, file:format_error(Reason)])
    end.

compile_engine(File) ->
    Options = [
      verbose,
      debug_info,
      return_errors,
      return_warnings
    ],
    Options1 = case otis_app:from_builddir() of
        false ->
            Options;
        true ->
            Src_Dir = os:getenv("srcdir"),
            Inc_Dir = filename:join([Src_Dir, "..", "include"]),
            [
              {i, Src_Dir},
              {i, Inc_Dir},
              {d, 'IN_SRC'}
              | Options
            ]
    end,
    Log_Error = fun({Line, Mod, Desc}, F) ->
        ?ERROR("~s:~b: ~s~n", [F, Line, Mod:format_error(Desc)]),
        F
    end,
    Log_Warning = fun({Line, Mod, Desc}, F) ->
        ?ERROR("~s:~b: Warning: ~s~n", [File, Line, Mod:format_error(Desc)]),
        F
    end,
    Log_Errors = fun
        ({".", Es}) -> lists:foldl(Log_Error, File, Es);
        ({F, Es})   -> lists:foldl(Log_Error, F, Es)
    end,
    Log_Warnings = fun
        ({".", Ws}) -> lists:foldl(Log_Warning, File, Ws);
        ({F, Ws})   -> lists:foldl(Log_Warning, F, Ws)
    end,
    case compile:file(File, Options1) of
        {ok, _} ->
            load_engine();
        {ok, _, []} ->
            load_engine();
        {ok, _, Warnings} ->
            ?ERROR("Warnings during engine build:~n", []),
            lists:foreach(Log_Warnings, Warnings),
            load_engine();
        {error, Errors, Warnings} ->
            ?ERROR("Failed to compile engine:~n", []),
            lists:foreach(Log_Errors, Errors),
            lists:foreach(Log_Warnings, Warnings),
            {error, engine_build_failure};
        error ->
            ?ERROR("Failed to compile engine~n", []),
            {error, engine_build_failure}
    end.

load_engine() ->
    code:purge(?ENGINE_MOD),
    case code:load_file(?ENGINE_MOD) of
        {module, _} ->
            ?INFO("Engine reloaded~n", []),
            ok;
        {error, Reason} ->
            ?ERROR("Failed to load engine: ~p~n", [Reason]),
            {error, engine_build_failure}
    end.
