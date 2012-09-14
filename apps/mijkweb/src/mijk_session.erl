-module(mijk_session).
-export([
            init/0,
            init_single/0,
            init_multinode/1,
            init/1,
            check_session/1,
            update_session/2,
            clean_up_sessions/0,
            clean_up_sessions_job/0,
            clean_up_sessions/1,
            clean_up_sessions_job/1,
            generate_session_uuid/0, generate_session_uuid/1,
            init_session/1,
            select/1,
            create_session/1,
            get_session_data/1,
            check_session_data/1,
            dirty_check_session_data/1,
            dirty_update_session/2,
            clean_up_sessions_job_inner/0,
            mysql_check_session_data/1,
            mysql_update_session/2,
            mcache_check_session_data/1,
            mcd_check_session_data/1,
            mcache_check_session_data_lock/1,
            mcache_check_session_data_lock/2,
            mcache_update_session_lock/2,
            mcache_update_session/2,
            mcd_update_session/2,
            dirty_init_session/1,
            mysql_init_session/1,
            mcache_init_session/1,
            dirty_create_session/1,
            mysql_create_session/1,
            mcache_create_session/1,
            mcd_create_session/1,
            mysql_to_erl/1,
            erl_to_mysql/1,
            get_instance/1,
            get_instance_cut/1
        ]).

-include("include/consts.hrl").
-include("../../deps/emysql/include/emysql.hrl").

init() ->
    timer:start(),
    case application:get_env(mijkweb, session_storage) of
        {ok, "mysql"}      ->
            case application:get_env(mijkweb, mysql_db_config) of
                {ok, [_|_] = L} ->
                    emysql:add_pool(sessionpool,
                        proplists:get_value("processes", L, 100),
                        proplists:get_value("user", L, "mijkweb"),
                        proplists:get_value("password", L, "mijkweb"),
                        proplists:get_value("host", L, "localhost"),
                        proplists:get_value("port", L, 3306),
                        proplists:get_value("database", L, "mijkweb"), utf8),
                        init("mysql"),
                        mijk_session:clean_up_sessions_job("mysql")
                ;_              -> exit("No mysql connection config")
            end;
        {ok, "memcached"} -> init("memcached");
        _                 ->
            init("mnesia"),
            mijk_session:clean_up_sessions_job()
    end.

init("memcached") -> unlink(element(2, mcd:start_link(localmcd, []))), mcache:start();
init("mysql")     ->
    emysql:execute(sessionpool, <<"create table IF NOT EXISTS mijkweb_session (
        guid varchar(50) not null,
        exp_date timestamp not null default now(),
        session_data blob default null,
        PRIMARY KEY(guid),
        key (exp_date)
        ) ENGINE InnoDB DEFAULT CHARACTER SET = utf8;">>);
init("mnesia")    ->
    case application:get_env(mijkweb, distribute_mode) of
        {ok, "multinode"} -> init_multinode(application:get_env(mijkweb, master_node))
        ;_                -> init_single()
    end.
    
init_multinode(undefined)       -> init_single();
init_multinode({ok, "self"})    -> init_single();
init_multinode({ok, Node}) when is_atom(Node)
                                ->
    case mnesia:table_info(schema, disc_copies) of
        []                ->
            rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            mnesia:add_table_copy(mijk_session, node(), ram_copies);
        L when is_list(L) ->
            case lists:member(node(), L) of
                true  -> ok;
                false ->
                    rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
                    mnesia:change_table_copy_type(schema, node(), disc_copies),
                    mnesia:add_table_copy(mijk_session, node(), ram_copies)
            end
    end;
init_multinode(_)               -> init_single().

init_single() ->
    case mnesia:table_info(schema, disc_copies) of
        []                -> create_schema();
        L when is_list(L) ->
            case lists:member(node(), L) of
                true  -> case mnesia:table_info(mijk_session, ram_copies) of
                            []                  -> create_session_table();
                            L1 when is_list(L1) ->
                                case lists:member(node(), L1) of
                                    true  -> {ok, "Already exists"};
                                    false -> create_session_table()
                                end
                         end;
                false -> create_schema()
            end
    end.
    

    
create_schema() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_session_table().


create_session_table() ->
    mnesia:create_table(mijk_session, [
        {ram_copies, [node()]},{disc_copies, []},{index, [expiration_date]},
        {attributes, [session_id, expiration_date, session_data]}]).

-spec init_session(binary()) -> any().
init_session(SSID) ->
    {Ms, Ss, _} = now(),
    mnesia:transaction(fun()-> mnesia:write({mijk_session, SSID, 1000000*Ms +Ss, []}) end).

-spec dirty_init_session(binary()) -> any().
dirty_init_session(SSID) ->
    {Ms, Ss, _} = now(),
    mnesia:dirty_write(mijk_session, {mijk_session, SSID, 1000000*Ms +Ss, []}).

-spec mysql_init_session(binary()) -> any().
mysql_init_session(SSID) ->
    emysql:execute(sessionpool, <<"insert into mijkweb_session values ('", SSID/binary, "', NOW(), NULL)">>).
    
-spec mcache_init_session(binary()) -> any().    
mcache_init_session(SSID) ->
    mcache:set(<<"mijkweb">>, SSID, erl_to_mysql([]), raw, {?SESSION_AGE, seconds}).

-spec mcd_init_session(binary()) -> any().    
mcd_init_session(SSID) -> mcd:ldo(set, SSID, erl_to_mysql([]), 0, ?SESSION_AGE).

select(all) ->
    mnesia:transaction(fun()-> mnesia:select(mijk_session, [{{mijk_session,'$1','$2','$3'}, [], ['$_']}]) end).
%-------------------------------------------------------------------------------

%
%
%
-spec create_session(integer()) -> binary().
create_session(Type) ->
    SSID = generate_session_uuid(Type),
    init_session(SSID), SSID.

%
%dirty version
%
-spec dirty_create_session(integer()) -> binary().
dirty_create_session(Type) ->
    SSID = generate_session_uuid(Type),
    dirty_init_session(SSID), SSID.

%
%mysql version
%
-spec mysql_create_session(integer()) -> binary().
mysql_create_session(Type) ->
    SSID = generate_session_uuid(Type),
    mysql_init_session(SSID), SSID.

%
%mcache version
%
-spec mcache_create_session(integer()|binary()) -> binary().
mcache_create_session(Type) when is_integer(Type)->
    SSID = generate_session_uuid(Type),
    mcache_init_session(SSID), SSID;
mcache_create_session(SSID) when is_binary(SSID)->
    lager:debug("BSSID: ~p", [SSID]),
    mcache_init_session(SSID), SSID.

-spec mcd_create_session(integer()|binary()) -> binary().
mcd_create_session(Type) when is_integer(Type)->
    SSID = generate_session_uuid(Type),
    mcd_init_session(SSID), SSID;
mcd_create_session(SSID) when is_binary(SSID)->
    lager:debug("BSSID: ~p", [SSID]),
    mcd_init_session(SSID), SSID.


%
%
%
-spec check_session(binary()) -> ok | nok.
check_session(SessionID) ->
    case mnesia:transaction(fun()-> mnesia:select(mijk_session, [{{mijk_session,'$1','$2','$3'}, [{'=:=','$1',SessionID}], ['$_']}]) end) of
        {atomic,[{mijk_session,SessionID, _, SessionData}]} ->
              {Ms, Ss, _} = now(),
              mnesia:transaction(fun()-> mnesia:write({mijk_session, SessionID, 1000000*Ms +Ss, SessionData}) end),
              ok
        ;_ -> nok
    end.
    
%
%
%
-spec check_session_data(binary()) -> nok | {ok, list()}.
check_session_data(SessionID) ->
    case mnesia:transaction(fun()-> mnesia:select(mijk_session, [{{mijk_session,'$1','$2','$3'}, [{'=:=','$1',SessionID}], ['$_']}]) end) of
        {atomic,[{mijk_session,SessionID, _, SessionData}]} ->
              {Ms, Ss, _} = now(),
              mnesia:transaction(fun()-> mnesia:write({mijk_session, SessionID, 1000000*Ms +Ss, SessionData}) end),
              {ok, SessionData}
        ;_ -> nok
    end.

%
% dirty version of check_session_data
%
-spec dirty_check_session_data(binary()) -> nok | {ok, list()}.
dirty_check_session_data(SessionID) ->
    case mnesia:dirty_select(mijk_session, [{{mijk_session,'$1','$2','$3'}, [{'=:=','$1',SessionID}], ['$_']}]) of
        [{mijk_session,SessionID, _, SessionData}] ->
              {Ms, Ss, _} = now(),
              mnesia:dirty_write({mijk_session, SessionID, 1000000*Ms +Ss, SessionData}),
              {ok, SessionData}
        ;_ -> nok
    end.

%
%
%
-spec mysql_check_session_data(binary()) -> nok | {ok, list()}.
mysql_check_session_data(SessionID) ->
    emysql:prepare(get_sessions, <<"select session_data from mijkweb_session where guid = ?">>),
    emysql:prepare(update_s_expdate, <<"update mijkweb_session set exp_date = NOW() where guid = ?">>),
    case emysql:execute(sessionpool, get_sessions, [SessionID]) of
        #result_packet{rows=[]}         -> nok;
        #result_packet{rows=[[S_Data]]} -> emysql:execute(sessionpool, update_s_expdate, [SessionID]), {ok, S_Data}
    end.

%
%
%
-spec mcache_check_session_data(binary()) -> nok | {ok, list()}.
mcache_check_session_data(SessionID) ->
    case mcache:get(<<"mijkweb">>, SessionID) of
        B when is_binary(B) -> {ok, B}
        ;_                  -> nok
    end.
    
-spec mcd_check_session_data(binary()) -> nok | {ok, list()}.    
mcd_check_session_data(SessionID) ->
    case mcd:get(SessionID) of
        {ok, B} when is_binary(B) -> {ok, B}
        ;_                        -> nok
    end.
%
%
%
-spec mcache_check_session_data_lock(binary()) -> nok | {ok, list()}.
mcache_check_session_data_lock(SessionID) ->
    {Ms,S,_} = now(),
    case mcache:set(<<"mijkweb">>, <<"lock-", SessionID/binary>>, erl_to_mysql([]), raw, {?MC_LOCK_AGE, seconds}, add) of
        {mc_async,0,ok} ->
            case ?MODULE:mcache_check_session_data(SessionID) of
                {ok, B} -> {ok, B};
                nok     -> ?MODULE:mcache_delete_lock(SessionID), nok
            end;
        {mc_async,0,{error,<<"CONNECTION DATA EXISTS">>}} -> %locked by another process, wait
            lager:warning("session locked: ~p", [SessionID]),
            timer:sleep(?MC_LOCK_SLEEP),
            ?MODULE:mcache_check_session_data_lock(SessionID, Ms*1000000+S)
    end.
%
%
%
-spec mcache_check_session_data_lock(binary(), integer()) -> nok | {ok, list()}.
mcache_check_session_data_lock(SessionID, Time) ->    
    {Ms,S,_} = now(),
    if   Ms*1000000+S-Time > ?MC_LOCK_TIMEOUT ->
            lager:warning("session ~p, lock timeout ~p", [SessionID, ?MC_LOCK_TIMEOUT]),
            {nok, lock_timeout}
        ;true ->
            case mcache:set(<<"mijkweb">>, <<"lock-", SessionID/binary>>, erl_to_mysql([]), raw, {?MC_LOCK_AGE, seconds}, add) of
                {mc_async,0,ok} ->
                    case ?MODULE:mcache_check_session_data(SessionID) of
                        {ok, B} -> {ok, B};
                        nok     -> mcache:delete(<<"mijkweb">>, <<"lock-", SessionID/binary>>), nok
                    end;
                {mc_async,0,{error,<<"CONNECTION DATA EXISTS">>}} -> %locked by another process, wait
                    lager:warning("session locked: ~p", [SessionID]),
                    timer:sleep(?MC_LOCK_SLEEP),
                    mcache_check_session_data_lock(SessionID, Time)
            end
    end.
%
%
%
-spec mcache_update_session(binary(), list()) -> ok | nok.
mcache_update_session(SessionID, SessionData) ->
    lager:debug("MUS-> ~p ~p ~n", [SessionData, SessionData]),
    mcache:set(<<"mijkweb">>, SessionID, erl_to_mysql(SessionData), raw, {?SESSION_AGE, seconds}),
    ok.

-spec mcd_update_session(binary(), list()) -> ok | nok.    
mcd_update_session(SessionID, SessionData) ->
    lager:debug("MUS-> ~p ~p ~n", [SessionData, SessionData]),
    mcd:ldo(set, SessionID, erl_to_mysql(SessionData), 0, ?SESSION_AGE),
    ok.
    
%
%
%
-spec mcache_update_session_lock(binary(), list()) -> ok | nok.
mcache_update_session_lock(SessionID, SessionData) ->
    mcache_update_session(SessionID, SessionData),
    mcache:delete(<<"mijkweb">>, <<"lock-", SessionID/binary>>),
    ok.
    
%
%
%
-spec mysql_update_session(binary(), list()) -> ok | nok.
mysql_update_session(SessionID, SessionData) ->
    emysql:prepare(update_s_update, <<"update mijkweb_session set session_data = ? where guid = ?">>),
    emysql:execute(sessionpool, update_s_update, [erl_to_mysql(SessionData), SessionID]),
    ok.
    
%
%
%
-spec update_session(binary(), list()) -> ok | nok.
update_session(SessionID, SessionData) ->
    case mnesia:transaction(fun()-> mnesia:select(mijk_session, [{{mijk_session,'$1','$2','$3'}, [{'=:=','$1',SessionID}], ['$_']}]) end) of
        {atomic,[{mijk_session,SessionID, _, _}]} ->
              {Ms, Ss, _} = now(),
              mnesia:transaction(fun()-> mnesia:write({mijk_session, SessionID, 1000000*Ms +Ss, SessionData}) end),
              ok
        ;_ -> nok
    end.

%
%
%
-spec dirty_update_session(binary(), list()) -> ok | nok.
dirty_update_session(SessionID, SessionData) ->
    case mnesia:dirty_select(mijk_session, [{{mijk_session,'$1','$2','$3'}, [{'=:=','$1',SessionID}], ['$_']}]) of
        {atomic,[{mijk_session,SessionID, _, _}]} ->
              {Ms, Ss, _} = now(),
              mnesia:dirty_write({mijk_session, SessionID, 1000000*Ms +Ss, SessionData}),
              ok
        ;_ -> nok
    end.

%
%
%
-spec get_session_data(binary()) -> nok | {ok, list()}.
get_session_data(SessionID) ->
    case mnesia:transaction(fun()-> mnesia:select(mijk_session, [{{mijk_session,'$1','$2','$3'}, [{'=:=','$1',SessionID}], ['$_']}]) end) of
        {atomic,[{mijk_session,SessionID, _, SessionData}]} -> {ok, SessionData}
        ;_                                                  -> nok
    end.
    
%
%clean up old, expired recordss
%
-spec clean_up_sessions() -> ok.
clean_up_sessions() ->
    {Ms, Ss, _} = now(),
    Now = 1000000*Ms + Ss - ?SESSION_AGE,
    case mnesia:transaction(fun()-> mnesia:select(mijk_session, [{{mijk_session,'$1','$2','$3'}, [{'<','$2',Now}], ['$_']}]) end) of
        {atomic,L} -> lists:foreach(fun(Record) -> mnesia:transaction(fun()-> mnesia:delete_object(Record) end) end, L)
        ;_         -> nok
    end,
    ok.

%
%
%
-spec clean_up_sessions(list()) -> ok.
clean_up_sessions("mysql") ->
    emysql:prepare(clean_sessions, <<"delete from mijkweb_session where exp_date < DATE_SUB(NOW(), INTERVAL ? MINUTE )">>),
    emysql:execute(sessionpool, clean_sessions, [round(?SESSION_AGE/60)]),
    ok.

%
% 
%
-spec clean_up_sessions_job(list()) -> ok.
clean_up_sessions_job("mysql") ->
    lager:debug("clean_up_sessions_job started", []),
    try clean_up_sessions("mysql") of
        ok  -> ok
    catch
        E:R -> lager:error("clean up sessions error: ~p ~p ~n", [E, R])
    end,
    timer:apply_after(300000, mijk_session, clean_up_sessions_job, ["mysql"]),
    ok.
    
%
% should work only on master node
%
-spec clean_up_sessions_job() -> ok.
clean_up_sessions_job() ->
    case {application:get_env(mijkweb, distribute_mode), application:get_env(mijkweb, master_node)} of
        {{ok, "single"}, _}               -> mijk_session:clean_up_sessions_job_inner();
        {{ok, "multinode"}, {ok, "self"}} -> mijk_session:clean_up_sessions_job_inner();
        {{ok, "multinode"}, {ok, Node}} when Node=:=node() -> mijk_session:clean_up_sessions_job_inner()
        ;_ -> lager:warning("clean_up_sessions_job not eligible for this nofe; ~p ", [node()])
    end.
    
%
% 
%
-spec clean_up_sessions_job_inner() -> ok.
clean_up_sessions_job_inner() ->    
    lager:debug("clean_up_sessions_job started", []),
    try mijk_session:clean_up_sessions() of
        ok  -> ok
    catch
        E:R -> lager:error("clean up sessions error: ~p ~p ~n", [E, R])
    end,
    timer:apply_after(300000, mijk_session, clean_up_sessions_job, []),
    ok.

%
%
%
erl_to_mysql(Term) -> base64:encode(term_to_binary(Term)).

mysql_to_erl(Bin)  -> binary_to_term(base64:decode(Bin)).

%
%
%
-spec generate_session_uuid() -> binary().
generate_session_uuid() -> list_to_binary(integer_to_list(random:uniform(10000000000) + 10000000)).
 
-spec generate_session_uuid(integer()) -> binary().
generate_session_uuid(1) -> list_to_binary(uuid:to_string(uuid:uuid4())).

%
% also we can include instance number in session guid: GUID_DDD (DDD -> 000-999 instance number)
%
-spec get_instance(binary()) -> list().
get_instance(SessionID) -> get_instance_ip(erlang:crc32(SessionID) rem ?SHARD_COUNT).

-spec get_instance_ip(integer()) -> list().
get_instance_ip(0) -> test1;
get_instance_ip(1) -> test1;
get_instance_ip(2) -> test1;
get_instance_ip(3) -> test2;
get_instance_ip(4) -> test2;
get_instance_ip(5) -> test2;
get_instance_ip(6) -> test2;
get_instance_ip(7) -> test3;
get_instance_ip(8) -> test3;
get_instance_ip(9) -> test3;
get_instance_ip(_) -> generic.

get_instance_cut(<<_:37/bytes, Inst/binary>>) -> get_instance_ip(list_to_integer(binary_to_list(Inst))).