-module(mijk_session).
-export([
            init/0,
            init_single/0,
            init_multinode/1,
            check_session/1,
            update_session/2,
            clean_up_sessions/0,
            clean_up_sessions_job/0,
            generate_session_uuid/0, generate_session_uuid/1,
            init_session/1,
            select/1,
            create_session/1,
            get_session_data/1,
            check_session_data/1,
            dirty_check_session_data/1,
            dirty_update_session/2
        ]).

-include("include/consts.hrl").

init() ->
    case application:get_key(mijkweb, distribute_mode) of
        {ok, "multinode"} -> init_multinode(application:get_key(mijkweb, master_node))
        ;_                -> init_single()
    end.
    
init_multinode(undefined)       -> init_single();
init_multinode({ok, "self"})    -> init_single();
init_multinode({ok, Node}) when is_atom(Node)
                                ->
    case mnesia:table_info(schema, disc_copies) of
        []                ->
            rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
            mnesia:change_table_copy_type(schema, node(), disc_copies);
        L when is_list(L) ->
            case lists:member(node(), L) of
                true  -> ok;
                false ->
                    rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
                    mnesia:change_table_copy_type(schema, node(), disc_copies)
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

init_session(SSID) ->
    {Ms, Ss, _} = now(),
    %mnesia:dirty_write(mijk_session, {mijk_session, SSID, 1000000*Ms +Ss, []}).
    mnesia:transaction(fun()-> mnesia:write({mijk_session, SSID, 1000000*Ms +Ss, []}) end).
    
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
-spec clean_up_sessions_job() -> ok.
clean_up_sessions_job() ->
    lager:debug("clean_up_sessions_job started", []),
    try mijk_session:clean_up_sessions() of
        ok  -> ok
    catch
        E:R -> lager:error("clean up sessions error: ~p ~p ~n", [E, R])
    end,
    timer:apply_after(600000, mijk_session, clean_up_sessions_job, []),
    ok.
%
%
%
-spec generate_session_uuid() -> binary().
generate_session_uuid() -> list_to_binary(integer_to_list(random:uniform(10000000000) + 10000000)).
 
-spec generate_session_uuid(integer()) -> binary().
generate_session_uuid(1) -> list_to_binary(uuid:to_string(uuid:uuid4())).
