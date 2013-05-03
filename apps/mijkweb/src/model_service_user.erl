-module(model_service_user).

-export([
    create_user/3,
    create_organization/0,
    delete_organization/1,
    check_sys_user/2,
    createappuser/3,
    createappuser/6,
    check_app_user/2,
    get_user_profile/1,
    get_key/2,
    set_key/3,
    del_key/2,
    check_sysuser_login_exists/1,
    updateappuser/5,
    deleteappuser/2,
    channels_list/2,
    channels_list/3
]).

-spec create_user(binary(), binary(), binary()) -> {ok, integer(), integer(), binary()} | {error, any()}.
create_user(Login, Password, Email) ->
    case check_sysuser_login_exists(Login) of
        0 ->
            {ok, OrgID, Guid} = create_organization(),
            emysql:prepare(create_sys_user, <<"insert into system_user (login, password, email, org_id) values (?,?,?,?)">>),
            case emysql:execute(mysqlpool, create_sys_user, [Login, Password, Email, OrgID]) of
                {ok_packet, _,1,Uid,_,_,_} -> {ok, Uid, OrgID, Guid};
                {error_packet, _,1062,_,_} -> {error, logininuse}
                ;Error ->
                    lager:error("Cannot create user ~p", [Error]),
                    catch(delete_organization(OrgID)),
                    {error, <<"cannot create user">>}
            end;
        _ -> {error, logininuse}
    end.

-spec check_sysuser_login_exists(binary()) -> 1|0.
check_sysuser_login_exists(Login) ->
    emysql:prepare(check_sysuser_login_exists, "select 1 from system_user where login = ?"),
    case emysql:execute(mysqlpool, check_sysuser_login_exists, [Login]) of
        {result_packet,_,_,[[1]],_} -> 1
        ;_                          -> 0
    end.

-spec create_organization() -> {ok, integer(), binary()}|{error, any()}. 
create_organization() ->
    emysql:prepare(create_org, <<"insert into organization (guid) values (?)">>),
    Guid = mijk_session:generate_session_uuid(1),
    case emysql:execute(mysqlpool, create_org, [Guid]) of
        {ok_packet, _,1, ID, _,_,_} -> {ok, ID, Guid}
        ;Error ->
            lager:error("Cannot create organization ~p", [Error]),
            {error, <<"cannot create organization">>}
    end.

delete_organization(OrgID) when is_integer(OrgID) ->
    emysql:prepare(delete_org, <<"delete from organization where id = ?">>),
    emysql:execute(mysqlpool, delete_org, [OrgID]).
    

-spec check_sys_user(binary(), binary()) -> {ok, integer(), integer()}|{error, any()}.
check_sys_user(Login, Password) when is_binary(Login),is_binary(Password)->
    emysql:prepare(check_sys_user, <<"select id, org_id from system_user where login=? and password=? limit 1">>),
    case emysql:execute(mysqlpool, check_sys_user, [Login, Password]) of
        {result_packet, _, _, [[Uid, OrgID]], _} -> {ok, Uid, OrgID}
        ;Error ->
            lager:error("Cannot find user ~p ~p ~p", [Login, Password, Error]),
            {error, <<"notfound">>}
    end.
    
-spec check_app_user(binary(), binary()) -> {ok, integer(), integer()}|{error, any()}.
check_app_user(Login, Password) when is_binary(Login),is_binary(Password)->
    emysql:prepare(check_app_user, <<"select id, sysaccid from app_user where login=? and password=? limit 1">>),
    case emysql:execute(mysqlpool, check_app_user, [Login, Password]) of
        {result_packet, _, _, [[Uid, SuiId]], _} -> {ok, Uid, SuiId}
        ;Error ->
            lager:error("Cannot find user ~p ~p ~p", [Login, Password, Error]),
            {error, <<"notfound">>}
    end.

-spec createappuser(binary(), binary(), integer()| binary()) -> {ok, integer()} | {error, any()}.
createappuser(Login, Password, SysAccountID) when is_integer(SysAccountID)-> 
    createappuser(Login, Password, SysAccountID, <<"WEBAPP">>, <<"">>, []);
createappuser(Login, Password, DevKey) when is_binary(DevKey)->
    createappuser(Login, Password, DevKey, <<"WEBAPP">>, <<"">>, []).

-spec createappuser(binary(), binary(), integer()| binary(), binary(), binary(), list()) -> {ok, integer()} | {error, any()}.
createappuser(Login, Password, SysAccountID, AppType, DeviceID, Opts) when is_integer(SysAccountID)->
    IPAddr = proplists:get_value(<<"ipaddr">>, Opts, <<"0.0.0.0">>),
    emysql:prepare(create_app_user, <<"insert into app_user (login, password, sysaccid, remote_ip) values (?, ?, ?, ?)">>),
    case emysql:execute(mysqlpool, create_app_user, [Login, Password, SysAccountID, IPAddr]) of
        {ok_packet, _,1,Uid,_,_,_}  ->
            emysql:execute(mysqlpool, 
                <<"insert into app_user_instance (appuserid, apptype, deviceid) values (?,?,?)">>, 
                [Uid, AppType, DeviceID]),
            {ok, Uid};
        {error_packet, _,1062,_, _} -> {error, logininuse}
        ;Error ->
            lager:error("Cannot create app user ~p", [Error]),
            {error, <<"cannot create app user">>}
    end;
createappuser(Login, Password, DevKey, AppType, DeviceID, Opts) when is_binary(DevKey)->
    case get_sys_accountid(DevKey) of
        {ok, SysAccountID} -> createappuser(Login, Password, SysAccountID, AppType, DeviceID, Opts);
        _                  -> {error, <<"cannot find developer guid">>}
    end.
 
-spec updateappuser(binary(), binary(), integer(), integer(), integer()) -> ok | {error, any()}.
updateappuser(Login, Password, Active, SysAccountID, AppUserID) ->
    case emysql:execute(mysqlpool, <<"call updateappuser(?,?,?,?,?);">>, [Login, Password, Active, SysAccountID, AppUserID]) of
        [{result_packet,_,_,[[Ret]],_}|_]  -> {ok, Ret}; 
        Ret -> Ret
    end.

-spec deleteappuser(integer(), integer()) -> ok | {error, any()}.
deleteappuser(SysAccountID, AppUserID) ->
    case emysql:execute(mysqlpool, <<"call deleteappuser(?,?);">>, [SysAccountID, AppUserID]) of
        [{result_packet,_,_,[[Ret]],_}|_]  -> {ok, Ret}; 
        Ret                                -> Ret
    end.

-spec get_sys_accountid(binary()) -> {ok, integer()}|{error, any()}.
get_sys_accountid(DevKey) ->
    emysql:prepare(get_sys_accountid, <<"select su.id from system_user su left join organization o on su.org_id=o.id where o.guid=? limit 1">>),
    case emysql:execute(mysqlpool, get_sys_accountid, [DevKey]) of
        {result_packet, _, _, [[SysAccountID]], _} -> {ok, SysAccountID}
        ;Error ->
            lager:error("Cannot find dev key ~p ~p", [DevKey, Error]),
            {error, <<"notfound">>}
    end.

get_user_profile(UserID) ->
    emysql:prepare(get_user_profile, <<"select app_user_count, storage_keys_count, max_app_users, max_keys_inst from system_user_profile where sysaccid = ?">>),
    case emysql:execute(mysqlpool, get_user_profile, [UserID]) of
        {result_packet, _, _, [[AUC, SKC, MAU, MKI]], _} ->
            {ok, {[{<<"user_registered">>, AUC},
                  {<<"keys_used">>, SKC},
                  {<<"max_users">>, MAU},
                  {<<"max_keys">>, MKI}]}}
        ;Error -> 
            lager:error("Cannot find user profile for ID: ~p -> ~p", [UserID, Error]),
            {error, <<"notfound">>}
    end.

set_key(AccountId, Key, Value) ->
    emysql:prepare(user_set_key, <<"insert into user_key_storage (sysaccid, `key`, `value`) values (?,?,?)">>),
    case emysql:execute(mysqlpool, user_set_key, [AccountId, Key, Value]) of
        {ok_packet,_,_,_,_,_,[]} -> ok;
        _ -> error 
    end.

get_key(AccountId, Key) ->
    emysql:prepare(user_get_key, <<"select `value` from user_key_storage where sysaccid = ? and `key` = ?">>),
    case emysql:execute(mysqlpool, user_get_key, [AccountId, Key]) of
        {result_packet, _, _, [[Value]], _} -> {ok, Value}
        ;Error ->
            lager:error("Cannot find dev key ~p ~p ~p", [AccountId, Key, Error]),
            {error, <<"notfound">>}
    end.

del_key(AccountId, Key) ->
    emysql:prepare(user_del_key, <<"delete from user_key_storage where sysaccid = ? and `key` = ?">>),
    case emysql:execute(mysqlpool, user_del_key, [AccountId, Key]) of
        {ok_packet,_,1,_,_,_,_} -> ok;
        {ok_packet,_,0,_,_,_,_} -> error
    end.

-spec app_users_list(integer()) -> list().
app_users_list(SysAccountID) -> app_users_list(SysAccountID, [], []).
-spec app_users_list(integer(), list(), list()) -> list().
app_users_list(SysAccountID, Where, Order) when is_integer(SysAccountID), is_list(Where), is_list(Order)->
    %
    % Where []
    %
    % Order
    ok.

channels_list(SysAccountID, Str) -> channels_list(SysAccountID, Str, 10).
channels_list(SysAccountID, Str, Limit) when is_integer(Limit) ->
    lager:debug("CL1 ~p ~p ~p", [SysAccountID, Str, Limit]),
    LimitB = list_to_binary(integer_to_list(Limit)),
    StrP = case Str of
        <<>> -> <<"%">>
        ;_   -> <<Str/binary,"%">>
    end,
    lager:debug("CL1 ~p ~p ~p", [SysAccountID, StrP, LimitB]),
    case emysql:execute(mysqlpool, 
        <<"select CONCAT(sysaccid,'_',id), login from app_user where sysaccid=? and login like ? limit ", 
            LimitB/binary >>, 
        [SysAccountID, StrP]) of
        {result_packet,_,_,R,_} -> [[list_to_binary(integer_to_list(SysAccountID)), <<"Common">>]] ++R
        ;_                      -> [[list_to_binary(integer_to_list(SysAccountID)), <<"Common">>]]
    end.
