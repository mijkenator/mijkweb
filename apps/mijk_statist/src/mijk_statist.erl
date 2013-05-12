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
    flush_reset/0
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
                ok     -> gen_server:cast(?STAT_WORKER, {wait_list_update, AccountID, erl_to_mysql(StatTerm)}), ok;
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
                        Ret when is_binary(Ret) -> mysql_to_erl(Ret)
                        ;_                      -> {error, baddata}
                      end;
                error  -> {error, overloaded}
            end;
        _ -> {error, "mijk_statist not running"} 
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


