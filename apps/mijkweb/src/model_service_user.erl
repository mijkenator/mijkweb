-module(model_service_user).

-export([
    create_user/3,
    create_organization/0,
    delete_organization/1,
    check_sys_user/2,
    createappuser/3,
    check_app_user/2
]).

-spec create_user(binary(), binary(), binary()) -> {ok, integer(), integer(), binary()} | {error, any()}.
create_user(Login, Password, Email) ->
    {ok, OrgID, Guid} = create_organization(),
    emysql:prepare(create_sys_user, <<"insert into system_user (login, password, email, org_id) values (?,?,?,?)">>),
    case emysql:execute(mysqlpool, create_sys_user, [Login, Password, Email, OrgID]) of
        {ok_packet, _,1,Uid,_,_,_} -> {ok, Uid, OrgID, Guid};
        {error_packet, _,1062,_,_} -> {error, logininuse}
        ;Error ->
            lager:error("Cannot create user ~p", [Error]),
            catch(delete_organization(OrgID)),
            {error, <<"cannot create user">>}
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
    emysql:prepare(create_app_user, <<"insert into app_user (login, password, sysaccid) values (?, ?, ?)">>),
    case emysql:execute(mysqlpool, create_app_user, [Login, Password, SysAccountID]) of
        {ok_packet, _,1,Uid,_,_,_}  -> {ok, Uid};
        {error_packet, _,1062,_, _} -> {error, logininuse}
        ;Error ->
            lager:error("Cannot create app user ~p", [Error]),
            {error, <<"cannot create app user">>}
    end;
createappuser(Login, Password, DevKey) when is_binary(DevKey)->
    case get_sys_accountid(DevKey) of
        {ok, SysAccountID} -> createappuser(Login, Password, SysAccountID);
        _                  -> {error, <<"cannot find developer guid">>}
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
