-module(riak_man).
-behaviour(application).
-behaviour(supervisor).

-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([init/1]).
%api functions
-export([
    ping/1, 
    list_keys/2,
    list_buckets/2,
    store/5,
    method/3]).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

start(_Type, _Args) ->
    supervisor:start_link({local, riak_man_sup}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    io:format("RM start ~n", []),
    {ok, Pools} = application:get_env(riak_man, pools),
    io:format("RM Pools: ~p ~n", [Pools]),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, riak_man_worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

ping(PoolName) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {ping})
    end).

list_keys(PoolName, Bucket) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {list_keys, Bucket})
    end).

list_buckets(PoolName, Params) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {list_buckets, Params})
    end).
    
store(PoolName, Bucket, Key, Body, BParams) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {store, Bucket, Key, Body, BParams})
    end).

method(PoolName, Method, Params) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {method, Method, Params})
    end).
    
    
