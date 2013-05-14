-module(mijk_statist_worker).
-behaviour(gen_server).

-export([start_link/1, start_link/0]).
-export([init/1, init/0, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {wait_list=[]}).
-define(SESSION_AGE, 300).

start_link(Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
start_link()     -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init()      -> init([]).
init(_Args) ->
    timer:send_after(600000, {sync}),
    {ok, #state{wait_list=[]}}.

handle_call({get_user_stats, AccountID}, _From, State) ->
    AIDb = list_to_binary(integer_to_list(AccountID)),
    Ret = case mcd:get(<<"sysacc_stat_", AIDb/binary>>) of
        {ok, B} when is_binary(B) -> B
        ;_                        -> 
            % get from database, put into memcache
            case emysql:execute(mysqlpool, <<"select raw_stat from sysacc_stat where sysaccid=?">>, [AccountID]) of
                 {result_packet, _, _, [[Bin]], _} -> 
                    AIDb = list_to_binary(integer_to_list(AccountID)),
                    mcd:ldo(set, <<"sysacc_stat_", AIDb/binary>>, Bin, 0, ?SESSION_AGE), Bin
                 ;Error                            -> lager:error("get online stats error: ~p", [Error]), {error, Error}
            end
    end,
    {reply, Ret, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({wait_list_update, AccountID, BinaryTerm}, #state{wait_list = WL} = State) ->
    NWL = case lists:member(AccountID, WL) of
        true  -> WL;
        false -> lists:append(AccountID, WL)
    end,
    AIDb = list_to_binary(integer_to_list(AccountID)),
    mcd:ldo(set, <<"sysacc_stat_", AIDb/binary>>, BinaryTerm, 0, ?SESSION_AGE),
    {noreply, State#state{wait_list = NWL}};
handle_cast({flush_reset}, #state{wait_list = WL} = State) ->
    Fn = fun(E) ->
        AIDb = list_to_binary(integer_to_list(E)),
        Key = <<"sysacc_stat_", AIDb/binary>>,
        case mcd:get(Key) of
            {ok, B} when is_binary(B) -> 
                mcd:lcd(delete, Key), 
                emysql:execute(mysqlpool, <<"call sync_sysacc_stat ?,?">>, [E, B])                
            ;_                        -> ok
        end
    end,
    lists:foreach(Fn, WL),
    emysql:execute(mysqlpool, <<"call flush_reset">>),
    {noreply, State#state{wait_list = []}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({sync}, #state{wait_list = WL} = State) ->
    Fn = fun(E) ->
        AIDb = list_to_binary(integer_to_list(E)),
        case mcd:get(<<"sysacc_stat_", AIDb/binary>>) of
            {ok, B} when is_binary(B) -> emysql:execute(mysqlpool, <<"call sync_sysacc_stat ?,?">>, [E, B])                
            ;_                        -> ok
        end
    end,
    spawn(fun() -> lists:foreach(Fn, WL) end),
    lager:debug("MIJKSTAT sync call", []),
    timer:send_after(600000, {sync}),
    {noreply, State#state{wait_list = []}};
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

