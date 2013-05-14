-module(mijk_statist).
-behaviour(application).
-behaviour(supervisor).

-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([init/1]).
%api functions
-export([ 
    wait_list_update/2,
    get_user_stats/1,
    is_process_alive/1,
    flush_reset/0,
    inc_stat_field/2, inc_stat_field/3,
    dec_stat_field/2, dec_stat_field/3,
    get_user_stats_safe/1,
    inc_online_users/2,
    inc_reg_today/2,
    inc_logins_today/2,
    inc_events_today/2,
    inc_key_using/2,
    dec_online_users/2,
    dec_reg_today/2,
    dec_logins_today/2,
    dec_events_today/2,
    dec_key_using/2,
    get_online_users/0
]).

-define(STAT_WORKER, mijk_statist_worker).
-define(LIMIT_PMQ, 1000).

start()             -> application:start(?MODULE).
stop()              -> application:stop(?MODULE).
start(_Type, _Args) -> supervisor:start_link({local, mijk_statist_sup}, ?MODULE, []).
stop(_State)        -> ok.

init([]) ->
    {ok, {{one_for_one, 10, 10}, [
        {?STAT_WORKER, {?STAT_WORKER, start_link, []}, 
                        permanent, 2000, worker, [?STAT_WORKER]}
    ]}}.

%----------------------------------------
-spec flush_reset() -> ok|nok.
flush_reset() ->
    case ?MODULE:is_process_alive(?STAT_WORKER) of
        true -> gen_server:cast(?STAT_WORKER, {flush_reset}), ok
        ;_   -> nok
    end.

-spec wait_list_update(integer(), any()) -> ok.
wait_list_update(AccountID, StatTerm)    ->
    case ?MODULE:is_process_alive(?STAT_WORKER) of
        true ->
            case check_overloads(?STAT_WORKER) of
                ok     -> gen_server:cast(?STAT_WORKER, {wait_list_update, AccountID, 
                                erl_to_mysql(process_stat_term(StatTerm))}), ok;
                error  -> ok     
            end
        ;_  -> ok
    end.

-spec get_user_stats(integer()) -> any().
get_user_stats(AccountID) ->
    case ?MODULE:is_process_alive(?STAT_WORKER) of
        true ->
            case check_overloads(?STAT_WORKER) of
                ok -> case gen_server:call(?STAT_WORKER, {get_user_stats, AccountID}) of
                        Ret when is_binary(Ret) -> {ok, mysql_to_erl(Ret)}
                        ;_                      -> {error, baddata}
                      end;
                error  -> {error, overloaded}
            end;
        _ -> {error, "mijk_statist not running"} 
    end.

-spec inc_stat_field(binary(), integer()) -> ok|error.
inc_stat_field(Binary, AccountID) -> inc_stat_field(Binary, AccountID, 1).
-spec inc_stat_field(binary(), integer(), integer()) -> ok|error.
inc_stat_field(Binary, AccountID, Inc) ->
    case get_user_stats(AccountID) of
        {ok, List} -> 
            NV = proplists:get_value(Binary, List, 0) + Inc, 
            wait_list_update(AccountID, [{Binary, NV}] ++ proplists:delete(Binary, List))
        ;_ -> error
    end.

-spec dec_stat_field(binary(), integer()) -> ok|error.
dec_stat_field(Binary, AccountID) -> dec_stat_field(Binary, AccountID, 1).
-spec dec_stat_field(binary(), integer(), integer()) -> ok|error.
dec_stat_field(Binary, AccountID, Dec) ->
    case get_user_stats(AccountID) of
        {ok, List} -> 
            NV = case proplists:get_value(Binary, List, 0) - Dec of
                NV0 when is_integer(NV0), NV0 > 0 -> NV0
                ;_ -> 0
            end,
            wait_list_update(AccountID, [{Binary, NV}] ++ proplists:delete(Binary, List))
        ;_ -> error
    end.

inc_online_users(AccountID, Inc) -> inc_stat_field(<<"online">>, AccountID, Inc).
inc_reg_today(AccountID, Inc)    -> inc_stat_field(<<"reg_today">>, AccountID, Inc).
inc_logins_today(AccountID, Inc) -> inc_stat_field(<<"logins_today">>, AccountID, Inc).
inc_events_today(AccountID, Inc) -> inc_stat_field(<<"events_today">>, AccountID, Inc).
inc_key_using(AccountID, Inc)    -> inc_stat_field(<<"key_using">>, AccountID, Inc).

dec_online_users(AccountID, Inc) -> dec_stat_field(<<"online">>, AccountID, Inc).
dec_reg_today(AccountID, Inc)    -> dec_stat_field(<<"reg_today">>, AccountID, Inc).
dec_logins_today(AccountID, Inc) -> dec_stat_field(<<"logins_today">>, AccountID, Inc).
dec_events_today(AccountID, Inc) -> dec_stat_field(<<"events_today">>, AccountID, Inc).
dec_key_using(AccountID, Inc)    -> dec_stat_field(<<"key_using">>, AccountID, Inc).

-spec stat_fields() -> list().
stat_fields() -> [<<"online">>,<<"reg_today">>,<<"logins_today">>,<<"events_today">>,<<"key_using">>].

get_user_stats_safe(AccountID) ->
    case get_user_stats(AccountID) of
        {ok, Stat} -> [{X, proplists:get_value(X, Stat, 0)} || X <- stat_fields()]
        ;_         -> [{X, 0} || X <- stat_fields()] 
    end.

-spec erl_to_mysql(any()) -> binary().
erl_to_mysql(Term) -> base64:encode(term_to_binary(Term)).

-spec mysql_to_erl(binary()) -> any().
mysql_to_erl(Bin)  -> binary_to_term(base64:decode(Bin)).

-spec check_overloads(atom()|pid())  -> ok|error|integer().
check_overloads(Proc) when is_atom(Proc) ->
    case erlang:whereis(Proc) of
        Pid when is_pid(Pid) -> check_overloads(Pid)
        ;_ ->
            lager:error("check_overload for bad registered name: ~p", [Proc]),
            error
    end;
check_overloads(Proc) when is_pid(Proc)  ->
    case erlang:process_info(Proc, message_queue_len) of
        {message_queue_len, I} when is_integer(I), I > ?LIMIT_PMQ -> 
            lager:error("process ~p overloaded ~p", [Proc, I]), I;
        {message_queue_len, I} when is_integer(I) -> ok;
        _                    -> error
    end.

-spec is_process_alive(atom()) -> true|false.
is_process_alive(ProcName) when is_atom(ProcName) ->
    case erlang:whereis(ProcName) of
        PPid when is_pid(PPid) -> erlang:is_process_alive(PPid)
        ;_ -> false
    end.

-spec process_stat_term(list()) -> list(). 
process_stat_term(StatTerm) ->
    case proplists:get_value(<<"online">>, StatTerm) of
        I when is_integer(I) ->
            case proplists:get_value(<<"maxonline">>, StatTerm) of
                J when is_integer(J), I>J -> [{<<"maxonline">>, I}] ++ proplists:delete(<<"maxonline">>, StatTerm);
                J when is_integer(J)      -> StatTerm
                ;_ -> [{<<"maxonline">>}, I] ++ StatTerm
            end
        ;_ -> StatTerm
    end.

-spec get_online_users() -> integer().
get_online_users() ->
   % get all dps_sessions
   % get all session waiters
   % group all unique waiters
   % check waiter process alive -> coun
    Sessions = ets:select(dps_sessions_manager:table(), ets:fun2ms(fun({_, SessProc}) -> SessProc end)),
    io:format("DPSSESS: ~p ~n", [Sessions]),
    
   0.
