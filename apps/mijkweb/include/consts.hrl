%% Types.
-type kv() :: {Name::binary(), Value::binary()}.
-type kvlist() :: [kv()].

-define(SESSION_AGE, 1200).
-define(SHARD_COUNT, 10).
-define(MC_LOCK_AGE, 5). % lock age , seconds
-define(MC_LOCK_SLEEP, 200). %lock sleep, millisec
-define(MC_LOCK_TIMEOUT, 1). %lock timeout, sec


-record(http_req, {
	%% Transport.
	socket = undefined :: undefined | inet:socket(),
	transport = undefined :: undefined | module(),
	connection = keepalive :: keepalive | close,

	%% Request.
	pid = undefined :: pid(),
	method = <<"GET">> :: binary(),
	version = {1, 1} :: cowboy_http:version(),
	peer = undefined :: undefined | {inet:ip_address(), inet:port_number()},
	host = undefined :: undefined | binary(),
	host_info = undefined :: undefined | cowboy_dispatcher:tokens(),
	port = undefined :: undefined | inet:port_number(),
	path = undefined :: binary(),
	path_info = undefined :: undefined | cowboy_dispatcher:tokens(),
	qs = undefined :: binary(),
	qs_vals = undefined :: undefined | list({binary(), binary() | true}),
	bindings = undefined :: undefined | cowboy_dispatcher:bindings(),
	headers = [] :: cowboy_http:headers(),
	p_headers = [] :: [any()], %% @todo Improve those specs.
	cookies = undefined :: undefined | [{binary(), binary()}],
	meta = [] :: [{atom(), any()}],

	%% Request body.
	body_state = waiting :: waiting | done | {stream, fun(), any(), fun()},
	multipart = undefined :: undefined | {non_neg_integer(), fun()},
	buffer = <<>> :: binary(),

	%% Response.
	resp_state = waiting :: locked | waiting | chunks | done,
	resp_headers = [] :: cowboy_http:headers(),
	resp_body = <<>> :: iodata()
		| {non_neg_integer(), fun(() -> {sent, non_neg_integer()})},

	%% Functions.
	onresponse = undefined :: undefined | cowboy_protocol:onresponse_fun(),
	urldecode :: {fun((binary(), T) -> binary()), T}
}).

