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
    method/3,
    put/3,
    get/5,get/3,
    get_value/5, get_value/3,
    get_bucket_properties/3, get_bucket_properties/1,
    set_bucket_backend/4]).

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

put(PoolName, Object, Params) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {put, Object, Params})
    end).

get(PoolName, Bucket, Key, Timeout, Options) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {get, Bucket, Key, Timeout, Options})
    end).
    
get_value(PoolName, Bucket, Key, Timeout, Options) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {get_value, Bucket, Key, Timeout, Options})
    end).

get(PoolName, Bucket, Key) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {get, Bucket, Key})
    end).
    
get_value(PoolName, Bucket, Key) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {get_value, Bucket, Key})
    end).

get_bucket_properties(Bucket) ->
    get_bucket_properties("127.0.0.1", "8098", Bucket).
get_bucket_properties(RiakIP, RiakPort, Bucket) when is_integer(RiakPort) -> 
    get_bucket_properties(RiakIP, integer_to_list(RiakPort), Bucket); 
get_bucket_properties(RiakIP, RiakPort, Bucket) ->
    RetStr = list_to_binary(os:cmd("curl -s http://"++ RiakIP ++":"++ RiakPort ++"/buckets/"++ Bucket  ++"/props | tail -1")),
    case jsx:is_json(RetStr) of
        true  -> jsx:decode(RetStr);
        false -> RetStr
    end.

set_bucket_backend(RiakIP, RiakPort, Bucket, BackendName) when is_integer(RiakPort)->
    set_bucket_backend(RiakIP, integer_to_list(RiakPort), Bucket, BackendName);
set_bucket_backend(RiakIP, RiakPort, Bucket, BackendName) ->
    os:cmd("curl -s -v -XPUT -H \"Content-Type: application/json\" -d '{\"props\":{\"backend\":\""++ 
    BackendName ++"\"}}' http://"++ RiakIP ++":"++ RiakPort ++"/riak/" ++ Bucket).

