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
    lager:debug("GUS: ~p", [AccountID]),
    AIDb = list_to_binary(integer_to_list(AccountID)),
    Ret = case mcd:get(<<"sysacc_stat_", AIDb/binary>>) of
        {ok, B} when is_binary(B) -> B
        ;_                        -> 
            % get from database, put into memcache
            case emysql:execute(mysqlpool, <<"select raw_stat from sysacc_stat where sysaccid=? and DATE(dtime) = DATE(NOW())">>, [AccountID]) of
                 {result_packet, _, _, [[Bin]], _} -> 
                    AIDb = list_to_binary(integer_to_list(AccountID)),
                    mcd:ldo(set, <<"sysacc_stat_", AIDb/binary>>, Bin, 0, ?SESSION_AGE), Bin;
                 {result_packet, _, _, [], _}      ->
                    BinDef = mijk_statist:get_default_stat_values(), 
                    mcd:ldo(set, <<"sysacc_stat_", AIDb/binary>>, BinDef, 0, ?SESSION_AGE), BinDef
                 ;Error                            -> lager:error("get online stats error: ~p", [Error]), {error, Error}
            end
    end,
    %Tar = timer:apply_after(2000, mijk_statist, update_user_stats, [?MODULE, AccountID, Ret]),
    Tar = timer:apply_after(2000, mijk_statist, update_user_stats_strict, [?MODULE, AccountID]),
    lager:debug("TAR ret: ~p", [Tar]),

    {reply, Ret, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({inner_stats_update, AccountID, UL, Stats}, State) ->
    ES = mijk_statist:mysql_to_erl(Stats),
    Fun = fun({S_k, S_v}, A) -> [{S_k, S_v}] ++ proplists:delete(S_k, A) end,
    BinaryTerm = mijk_statist:erl_to_mysql(lists:foldl(Fun, ES, UL)),
    gen_server:cast(?MODULE, {wait_list_update, AccountID, BinaryTerm}),
    {noreply, State};
handle_cast({inner_stats_strict_update, AccountID, UL}, State) ->
    AIDb = list_to_binary(integer_to_list(AccountID)),
    Ret = case mcd:get(<<"sysacc_stat_", AIDb/binary>>) of
        {ok, B} when is_binary(B) -> B
        ;_ -> mijk_statist:get_default_stat_values()
    end,
    ES = mijk_statist:mysql_to_erl(Ret),
    Fun = fun({S_k, S_v}, A) -> [{S_k, S_v}] ++ proplists:delete(S_k, A) end,
    BinaryTerm = mijk_statist:erl_to_mysql(lists:foldl(Fun, ES, UL)),
    gen_server:cast(?MODULE, {wait_list_update, AccountID, BinaryTerm}),
    {noreply, State};
handle_cast({wait_list_update, AccountID, BinaryTerm}, #state{wait_list = WL} = State) ->
    NWL = case lists:member(AccountID, WL) of
        true  -> WL;
        false -> lists:append([AccountID], WL)
    end,
    AIDb = list_to_binary(integer_to_list(AccountID)),
    lager:debug("-------------WLU: ~p ~p ~p", [AccountID, AIDb, mijk_statist:mysql_to_erl(BinaryTerm)]),
    mcd:ldo(set, <<"sysacc_stat_", AIDb/binary>>, BinaryTerm, 0, ?SESSION_AGE),
    {noreply, State#state{wait_list = NWL}};
handle_cast({flush_reset}, #state{wait_list = WL} = State) ->
    lager:debug("flush_reset started ~p", [WL]),
    Fn = fun(E) ->
        AIDb = list_to_binary(integer_to_list(E)),
        Key  = <<"sysacc_stat_", AIDb/binary>>,
        lager:debug("flush_reset 1 ~p ~p", [AIDb, Key]),
        case mcd:get(Key) of
            {ok, B} when is_binary(B) -> 
                lager:debug("flush_reset 2 ~p ~p", [Key, B]),
                mcd:ldo(delete, Key), 
                emysql:execute(mysqlpool, <<"call sync_sysacc_stat(?,?);">>, [E, B])                
            ;_                        -> ok
        end
    end,
    lists:foreach(Fn, WL),
    emysql:execute(mysqlpool, <<"call flush_reset()">>),
    {noreply, State#state{wait_list = []}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({sync}, #state{wait_list = WL} = State) ->
    Fn = fun(E) ->
        AIDb = list_to_binary(integer_to_list(E)),
        case mcd:get(<<"sysacc_stat_", AIDb/binary>>) of
            {ok, B} when is_binary(B) -> emysql:execute(mysqlpool, <<"call sync_sysacc_stat(?,?)">>, [E, B])                
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

