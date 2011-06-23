%% -------------------------------------------------------------------
%% Rewrite state.
%% -------------------------------------------------------------------

-record(state, {
    rule_name    = "",

    %% Request.
    client_ip    = undefined,
    client_port  = undefined,
    server_name  = "",
    server_ip    = undefined,
    server_port  = undefined,
    http_ver     = {1, 1},
    method       = "GET",
    scheme       = "",
    path         = "",
    query_str    = "",
    fragment     = "",
    headers      = [],
    query_parsed = false,

    %% Response (filled in case of a response or redirect).
    response     = false,
    code         = 301,
    reason       = undefined,
    rheaders     = [],
    rbody        = undefined,

    %% User variables.
    vars         = dict:new(),

    other_state
  }).

%% -------------------------------------------------------------------
%% Variables.
%% -------------------------------------------------------------------

-record(var, {
    prefix  = undefined,
    name    = "",
    attr    = undefined,
    filters = []
  }).

-define(IS_VAR_WRITABLE(V), (
    V#var.prefix /= undefined orelse (
      V#var.name /= "CLIENT_PORT" andalso
      V#var.name /= "SERVER_PORT" andalso
      V#var.name /= "RULE" andalso
      V#var.name /= "STATE" andalso
      V#var.name /= "RESULT" andalso
      V#var.name /= "USER_VARS"
    )
  )).

%% -------------------------------------------------------------------
%% Operators.
%% -------------------------------------------------------------------

-record(op_log, {
    mod        = ?MODULE,
    format_str = "",
    line,
    col
  }).

-record(op_eq, {
    mod   = ?MODULE,
    var   = "",
    value = "",
    type  = string,
    line,
    col
  }).

-record(op_match, {
    mod           = ?MODULE,
    var           = undefined,
    regex         = undefined,
    compile_flags = [],
    match_flags   = [],
    captures      = [],
    line,
    col
  }).

-record(op_set, {
    mod   = ?MODULE,
    var   = "",
    value = "",
    type  = string,
    line,
    col
  }).

-record(op_subst, {
    mod           = ?MODULE,
    var           = undefined,
    regex         = undefined,
    compile_flags = [],
    match_flags   = [],
    value         = undefined,
    line,
    col
  }).

-record(op_not, {
    mod      = ?MODULE,
    subexprs = [],
    line,
    col
  }).

-record(op_all, {
    mod      = ?MODULE,
    subexprs = [],
    line,
    col
  }).

-record(op_any, {
    mod      = ?MODULE,
    subexprs = [],
    line,
    col
  }).

-record(op_goto, {
    mod      = ?MODULE,
    target   = next,
    line,
    col
  }).

-record(op_response, {
    mod      = ?MODULE,
    code     = 301,
    reason,
    line,
    col
  }).

-define(EXPR_MOD(E), element(#op_eq.mod, E)).

%% -------------------------------------------------------------------
%% Ruleset.
%% -------------------------------------------------------------------

-record(hooks, {
    rule_init   = [],
    if_continue = [],
    if_final    = [],
    if_abort    = [],
    line,
    col
  }).

-record(rule, {
    name   = undefined,
    desc   = undefined,
    exprs  = [],
    hooks  = #hooks{},
    consts = dict:new(),
    line,
    col
  }).

-record(ruleset, {
    rules  = [],
    hooks  = #hooks{},
    consts = dict:new(),
    file,
    yaml
  }).

%% -------------------------------------------------------------------
%% Logging.
%% -------------------------------------------------------------------

%-define(ERROR(Format, Args),   io:format(standard_error, Format, Args)).
%-define(WARNING(Format, Args), io:format(standard_error, Format, Args)).
%-define(INFO(Format, Args),    io:format(standard_error, Format, Args)).
%-define(DEBUG(Format, Args),   io:format(standard_error, Format, Args)).
-define(ERROR(Format, Args),   otis_log:error_msg(Format, Args)).
-define(WARNING(Format, Args), otis_log:warning_msg(Format, Args)).
-define(INFO(Format, Args),    otis_log:info_msg(Format, Args)).
-define(DEBUG(Format, Args),   otis_log:debug_msg(Format, Args)).
