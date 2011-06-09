-module(otis).

-include("otis.hrl").

%% Public API.
-export([
    reload_engine/0,
    reload_engine/1
  ]).

-define(ENGINE_FILE, "otis_reqrw_engine.erl").

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

gen_engine(Ruleset) ->
    Code = otis_tpl:gen(Ruleset),
    Dir  = otis_app:get_param(engine_save_dir),
    File = case Dir of
        "" -> ?ENGINE_FILE;
        _  -> filename:join([otis_utils:expand_path(Dir), ?ENGINE_FILE])
    end,
    case file:open(File, [write, {encoding, unicode}]) of
        {ok, FD} ->
            file:write(FD, Code),
            file:close(FD),
            compile_engine(File);
        {error, Reason} ->
            ?ERROR("Failed to save request rewriting engine "
              "to \"~s\": ~s", [File, file:format_error(Reason)])
    end.

compile_engine(File) ->
    Options = [
      binary,
      verbose,
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
        {ok, Mod, Bin} ->
            load_engine(File, Mod, Bin);
        {ok, Mod, Bin, []} ->
            load_engine(File, Mod, Bin);
        {ok, Mod, Bin, Warnings} ->
            ?ERROR("Warnings during engine build:~n", []),
            lists:foreach(Log_Warnings, Warnings),
            load_engine(File, Mod, Bin);
        {error, Errors, Warnings} ->
            ?ERROR("Failed to compile engine:~n", []),
            lists:foreach(Log_Errors, Errors),
            lists:foreach(Log_Warnings, Warnings),
            {error, engine_build_failure};
        error ->
            ?ERROR("Failed to compile engine~n", []),
            {error, engine_build_failure}
    end.

load_engine(File, Mod, Bin) ->
    case code:load_binary(Mod, File, Bin) of
        {module, Mod} ->
            ?INFO("Request rewriting engine reloaded~n", []),
            ok;
        {error, Reason} ->
            ?ERROR("Failed to load engine: ~p~n", [Reason]),
            {error, engine_build_failure}
    end.
