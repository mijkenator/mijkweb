-module(mijk_session_manager).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([
    start_link/0,
    start_link/1,
    get_instance/1,
    get_instance/2,
    update_mc_pools/1
]).
-compile(export_all). % comment after debug

-behaviour(gen_server).

-record(state, {
    mcached_pools = [generic],
    default_pool = generic,
    mcached_pools_size = 1
}).


start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, #state{}, []).

-spec start_link(list())  -> {ok,pid()} | ignore | {error,term()}.
start_link(Mcached_Pools) when is_list(Mcached_Pools) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
        #state{mcached_pools = Mcached_Pools, mcached_pools_size = length(Mcached_Pools)}, []).

-spec get_instance(binary()) -> term().
get_instance(SSID) -> gen_server:call(?MODULE, {get_instance, SSID}).

-spec get_instance(binary(), integer()) -> term().
get_instance(SSID, _) -> virt_ms_dispatcher:get_instance(erlang:crc32(SSID) rem virt_ms_dispatcher:pool_length() + 1).

-spec update_mc_pools(list())  -> ok.
update_mc_pools(Mcached_Pools) -> gen_server:cast(?MODULE, {update_mc_pools, Mcached_Pools}).

%-------------------------------------------------------------------------------

init(#state{mcached_pools = Mcached_Pools, default_pool = DefaultPool} = State) ->
    ?MODULE:generate_virt_ms_dispatcher(Mcached_Pools, DefaultPool),
    {ok, State}.

handle_cast({update_mc_pools, Mcached_Pools}, State) ->
    ?MODULE:generate_virt_ms_dispatcher(Mcached_Pools, State#state.default_pool),
    {noreply, State#state{mcached_pools = Mcached_Pools, mcached_pools_size = length(Mcached_Pools)}};
handle_cast(Msg, State) ->
    lager:error("~p unknown cast !!!! ~p ~p ~n", [?MODULE, Msg, State]),
    {noreply, State}.

handle_call({get_instance, SSID}, _, #state{mcached_pools = MPs, mcached_pools_size = MPL} = State) ->
    Instance = lists:nth(erlang:crc32(SSID) rem MPL + 1, MPs),
    {reply, {ok, Instance}, State};
handle_call(_Msg, _Caller, State) ->
    lager:error("~p unknown call !!!! ~p ~p ~p ~n", [?MODULE, _Msg, _Caller, State]),
    {noreply, State}.
    
handle_info(Msg, State) ->
    lager:error("~p unknown info !!!! ~p ~p ~n", [?MODULE, Msg, State]),
    {noreply, State}.
    
terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%-------------------------------------------------------------------------------

-spec generate_virt_ms_dispatcher(list(), atom())     -> ok.
generate_virt_ms_dispatcher(Mcached_Pools, DefaultPool)
    when is_list(Mcached_Pools), is_atom(DefaultPool) ->
    Code = lists:flatten(
        [ "-module(virt_ms_dispatcher).\n-author('mijkenator@gmail.com').\n"
          "-export([get_instance/1, pool_length/0]).% DO NOT EDIT.\n\n",
        [ io_lib:format("get_instance(~p) -> ~p;~n", [Number, PoolName]) ||
            {PoolName, Number} <- lists:zip(Mcached_Pools, lists:seq(1, length(Mcached_Pools))) ],
        io_lib:format("get_instance(_) -> ~p.~n", [DefaultPool]),
        io_lib:format("pool_length() -> ~p.~n", [length(Mcached_Pools)]) ]),
    {M, B} = dynamic_compile:from_string(Code),
    code:load_binary(M, "", B), ok.
    
    