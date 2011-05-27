-module(sample).

-export([
    arg_rewrite/1
  ]).

arg_rewrite(ARG) ->
    %% Conversion: ARG -> State
    State = ARG,
    try
        State1 = eval(State),
        %% Reverse conversion: State -> ARG
        ARG1 = State1,
        ARG1
    catch
        throw:abort -> ARG;
        _:_         -> 500
    end.

eval(State) ->
    rule_1(State).

%% -------------------------------------------------------------------
%% Rule #1.
%% -------------------------------------------------------------------

rule_1(State) ->
    try
        %% By default, stop here if the rule run successfully.
        r1_1_eq(State) %% XXX FIRST OP.
    catch
        %% Non-terminal rule.
        throw:abort            -> rule_2(State);
        throw:{first, State1}  -> rule_1(State1);
        throw:{next, State1}   -> rule_2(State1);
        throw:{rule_2, State1} -> rule_2(State1);
        throw:{stop, State1}   -> State1
    end.

%% Rule #1 > Op #1 (eq).
r1_1_eq(#state{path = "/user/profile.php"} = State) ->
    r1_2_set(State); %% XXX NEXT OP.
r1_1_eq(State) ->
    throw(abort).

%% Rule #1 > Op #2 (set).
r1_2_set(State) ->
    State1 = State#state{path = "/user/profile.php"},
    State1. %% XXX LAST OP.

%% -------------------------------------------------------------------
%% Rule #2.
%% -------------------------------------------------------------------

rule_2(State) ->
    try
        r2_1_any(State) %% XXX FIRST OP.
    catch
        %% Terminal rule.
        %% throw:abort -> forward abort;
        throw:{first, State1} -> rule_1(State);
        throw:{next, State} -> throw(abort);
        throw:{rule_1, State1} -> rule_1(State);
        throw:{stop, State1} -> State1
    end.

%% Rule #2 > Op #1 (any).
r2_1_any(State) ->
    r2_1_any_1(State). %% XXX FIRST SUB-OP.

r2_1_any_1(State) ->
    try
        r2_1_any_1_all(State) %% XXX SUB-SUB-OP.
    catch
        throw:abort -> r2_1_any_2(State) %% XXX NEXT SUB-OP.
    end;
r2_1_any_2(State) ->
    try
        r2_1_any_2_all(State) %% XXX SUB-SUB-OP.
    catch
        throw:abort -> r2_1_any_abort(State) %% XXX NO SUCCESSFULL SUB-OP.
    end;
r2_1_any_abort(State) ->
    throw(abort).

%% Rule #2 > Op #1 (any) > Op #1 (all).
r2_1_any_1_all(State) ->
    r2_1_any_1_all_1_regex(State). %% XXX FIRST SUB-SUB-SUB-OP.

%% Rule #2 > Op #1 (any) > Op #1 (all) > Op #1 (regex).
r2_1_any_1_all_1_regex(#state{path = Path} = State) ->
    Regex   = <<"...">>,
    Options = [{capture, all_but_first, list}],
    case re:run(Path, Regex, Options) of
        {match, Captures} ->
            r2_1_any_1_all_1_regex2(State, Captures);
        nomatch ->
            throw(abort)
    end.
r2_1_any_1_all_1_regex2(State, [Path | Rest]) ->
    State1 = State#state{path = Path},
    r2_1_any_1_all_1_regex2(State1, Rest);
r2_1_any_1_all_1_regex2(State, []) ->
    r2_1_any_1_all_1_set(State). %% XXX NEXT SUB-SUB-SUB-OP.

%% Rule #2 > Op #1 (any) > Op #1 (all) > Op #2 (set).
r2_1_any_1_all_1_set(#state{vars_query = undefined} = State) ->
    State1 = parse_query(State),
    r2_1_any_1_all_1_set(State1);
r2_1_any_1_all_1_set(#state{vars_query = Query} = State) ->
    r2_1_any_1_all_1_set2(State, Query, [], false).

r2_1_any_1_all_1_set2(State, [{"univ", _} | Rest], Query, false) ->
    Query1 = [{"univ", "housing"} | Query],
    r2_1_any_1_all_1_set2(State, Rest, Query1, true);
r2_1_any_1_all_1_set2(State, [{"univ", _} | Rest], Query, false) ->
    r2_1_any_1_all_1_set2(State, Rest, Query, false);
r2_1_any_1_all_1_set(State, [Param | Rest], Query, Already_Set) ->
    Query1 = [Param | Query],
    r2_1_any_1_all_1_set2(State, Rest, Query1, Already_Set);
r2_1_any_1_all_1_set2(State, [], Query, false) ->
    Query1 = [{"univ", "housing"} | Query],
    r2_1_any_1_all_1_set2(State, [], Query1, true);
r2_1_any_1_all_1_set2(State, [], Query, true) ->
    State1 = State#state{vars_query = Query},
    r2_1_any_1_all_2_regex(State). %% XXX NEXT SUB-SUB-OP, ie. SUB-SUB-SUB-OP.

%% Rule #2 > Op #1 (any) > Op #2 (all) > Op #1 (regex).
r2_1_any_1_all_2_regex(State) ->
    %% /idem/
    r2_1_any_1_all_2_set(State). %% XXX NEXT SUB-SUB-SUB-OP.

%% Rule #2 > Op #1 (any) > Op #2 (all) > Op #2 (set).
r2_1_any_1_all_2_set(State) ->
    %% /idem/
    r2_1_any_2_all(State). %% XXX PARENT SUB-OP.

%% Rule #2 > Op #1 (any) > Op #2 (all) > Op #2 (set).
r2_1_any_2_all(State) ->
    %% /idem/
    r2_2_any(State). %% XXX PARENT OP.

