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
