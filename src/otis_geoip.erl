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

-module(otis_geoip).

-include("otis.hrl").

-behaviour(gen_server).

-export([
    start_link/0,
    get_country_iso_code/1
  ]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

-record(geoip_state, {
    db
  }).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_country_iso_code(IP_Addr) ->
    try
        gen_server:call(?MODULE, {get_country_iso_code, IP_Addr})
    catch
        exit:{noproc, _} ->
            undefined
    end.

%% -------------------------------------------------------------------
%% gen_server's callbacks.
%% -------------------------------------------------------------------

init(_) ->
    State  = #geoip_state{},
    State1 = reload_db(State),
    {ok, State1}.

handle_call({get_country_iso_code, _}, _,
  #geoip_state{db = undefined} = State) ->
    {reply, {error, no_geoip_db_loaded}, State};
handle_call({get_country_iso_code, IP_Addr}, _,
  #geoip_state{db = DB} = State) ->
    try
        case maxminddb:find(DB, IP_Addr) of
            {ok, undefined} ->
                {reply, undefined, State};
            {ok, Record} ->
                {_, Country} = lists:keyfind(<<"country">>, 1, Record),
                {_, ISO_Code} = lists:keyfind(<<"iso_code">>, 1, Country),
                {reply, binary_to_list(ISO_Code), State}
        end
    catch
        _:Exception ->
            Stacktrace = erlang:get_stacktrace(),
            ?ERROR("GeoIP lookup crash: ~p~n~p~n", [Exception, Stacktrace]),
            {reply, undefined, State}
    end.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

reload_db(State) ->
    case otis_app:get_param(geoip_db) of
        none ->
            State;
        File ->
            case maxminddb:open(File) of
                {error, Reason} ->
                    ?ERROR("Failed to load GeoIP database \"~s\": ~p~n",
                      [File, Reason]),
                    State;
                DB ->
                    State#geoip_state{db = DB}
            end
    end.
