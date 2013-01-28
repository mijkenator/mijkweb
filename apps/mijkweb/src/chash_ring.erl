-module(chash_ring).
-behaviour(gen_server).

-export([
  % public API
  lookup/1,
  start_link/1,
  get_ring/0,

  % gen_server callbacks
  code_change/3,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  init/1,
  terminate/2
]).

-record(state, { ring, nodes }).

lookup(Key) -> gen_server:call(?MODULE, {lookup, Key}).

get_ring()  -> gen_server:call(?MODULE, {get_ring}).

start_link([RingSize, Points]) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [RingSize, Points], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%

handle_call({lookup, Key}, _From, #state{ ring = Ring } = State) ->
  {reply, chash:successors(chash:key_of(Key), Ring), State};

handle_call({get_ring}, _, #state{ ring = Ring } = State) ->
  {reply, Ring, State};

handle_call(_Request, _From, State) ->
  {noreply, State}.

%%%

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

init([RingSize, Points]) ->
    io:format("init: ~p ~p ~n", [RingSize, Points]),
    Fun = fun(K, R) -> tests:chash_add_node(R, K)  end,
    Ring = lists:foldl(Fun, chash:fresh(RingSize, 1), lists:seq(2, Points)),
    io:format("init ring: ~p ~n", [Ring]),
    {ok, #state{ ring = Ring }}.

terminate(_Reason, _State) ->
  ok.

