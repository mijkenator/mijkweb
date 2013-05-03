-module(model_admin_user_grid).

-include("include/consts.hrl").
-include("include/mijkweb_consts.hrl").

-export([
    select/5,
    select/6,
    select_appuserdetails/6,
    app_user_count/1,
    app_user_count/2,
    get_extra_sql/1,
    split_in_statement/1
]).

select(SysAccountID, Rows, Page, SIdx, SOrd) -> select(SysAccountID, Rows, Page, SIdx, SOrd, []).
select(SysAccountID, Rows, Page, SIdx, SOrd, Extra) ->
    {ExtraSql, ExtraParam} = get_extra_sql(Extra),
    lager:debug("MAUG1 ~p ~p", [ExtraSql, ExtraParam]),
    Rowsb    = list_to_binary(integer_to_list(Rows)),
    Offset   = list_to_binary(integer_to_list((Page-1)*Rows)),
    case emysql:execute(mysqlpool, 
            <<"select id,active,login,password,remote_ip,register_time,last_login_time from app_user where sysaccid=? ",
                ExtraSql/binary, "order by ",SIdx/binary," ",
                    SOrd/binary, " limit ", Offset/binary, ",", Rowsb/binary>>, [SysAccountID | ExtraParam]) of
        {result_packet, _, _, Ret, _} -> {app_user_count(SysAccountID, {ExtraSql, ExtraParam}), Ret}
        ;Error ->
            lager:debug("MAUGERR ~p", [Error]),
            lager:error("admin_select_user error ~p", [Error]),
            {error, <<"notfound">>}
    end.

app_user_count(SysAccountID) -> app_user_count(SysAccountID, {<<" AND 1 = ? ">>, 1}). 
app_user_count(SysAccountID, {ExtraSql, ExtraParam}) -> 
    case emysql:execute(mysqlpool, <<"select count(*) from app_user where sysaccid=? ", ExtraSql/binary>>, 
                                        [SysAccountID | ExtraParam]) of
        {result_packet, _, _, [[Ret]], _} -> Ret
        ;Error ->
            lager:error("admin_select_user error ~p", [Error]),
            {error, <<"notfound">>}
    end.

select_appuserdetails(SysAccountID, Rows, Page, SIdx, SOrd, Extra) ->
    AppUserID = proplists:get_value(<<"appuserid">>, Extra, 0),
    {ExtraSql, ExtraParam} = get_extra_sql(Extra),
    lager:debug("MAUG1 ~p ~p", [ExtraSql, ExtraParam]),
    Rowsb    = list_to_binary(integer_to_list(Rows)),
    Offset   = list_to_binary(integer_to_list((Page-1)*Rows)),
    case emysql:execute(mysqlpool, 
            <<"select aui.id, aui.appuserid, aui.apptype, aui.deviceid from app_user au left join app_user_instance aui on au.id=aui.appuserid where au.sysaccid=? and au.id=? ",
                ExtraSql/binary, "order by ",SIdx/binary," ",
                    SOrd/binary, " limit ", Offset/binary, ",", Rowsb/binary>>, [SysAccountID, AppUserID | ExtraParam]) of
        {result_packet, _, _, Ret, _} -> {appuserdetails_count(SysAccountID, AppUserID, {ExtraSql, ExtraParam}), Ret}
        ;Error ->
            lager:debug("MAUGERR ~p", [Error]),
            lager:error("admin_select_appuserdetails error ~p", [Error]),
            {error, <<"notfound">>}
    end.

appuserdetails_count(SysAccountID, AppUserID, {ExtraSql, ExtraParam}) -> 
    case emysql:execute(mysqlpool, 
    <<"select count(*) from app_user au left join app_user_instance aui on au.id=aui.appuserid where au.sysaccid=? and au.id=? ", 
            ExtraSql/binary>>, [SysAccountID, AppUserID | ExtraParam]) of
        {result_packet, _, _, [[Ret]], _} -> Ret
        ;Error ->
            lager:error("admin_select_user error ~p", [Error]),
            {error, <<"notfound">>}
    end.

-spec get_extra_sql(list()) -> {binary(), list()}.
get_extra_sql([])           -> {<<" AND 1 = ? ">>, [1]};
get_extra_sql(Extra)        -> get_extra_sql_search(proplists:get_value(search, Extra, undefined)).

get_extra_sql_search([<<>>|_])                     -> {<<" AND 1 = ? ">>, [1]};
get_extra_sql_search([SearchField, V, <<"eq">>])   -> {<<" AND ", SearchField/binary, " =  ? ">>, [V]};
get_extra_sql_search([SearchField, V, <<"ne">>])   -> {<<" AND ", SearchField/binary, " !=  ? ">>, [V]};
get_extra_sql_search([SearchField, V, <<"bn">>])   -> {<<" AND ", SearchField/binary, " not like  ? ">>, [<<V/binary,"%">>]};
get_extra_sql_search([SearchField, V, <<"bw">>])   -> {<<" AND ", SearchField/binary, " like  ? ">>, [<<V/binary,"%">>]};
get_extra_sql_search([SearchField, V, <<"ew">>])   -> {<<" AND ", SearchField/binary, " like  ? ">>, [<<"%",V/binary>>]};
get_extra_sql_search([SearchField, V, <<"en">>])   -> {<<" AND ", SearchField/binary, " not like ? ">>, [<<"%",V/binary>>]};
get_extra_sql_search([SearchField, V, <<"cn">>])   -> {<<" AND ", SearchField/binary, " like  ? ">>, [<<"%",V/binary,"%">>]};
get_extra_sql_search([SearchField, V, <<"nc">>])   -> {<<" AND ", SearchField/binary, " not like ? ">>, [<<"%",V/binary,"%">>]};
get_extra_sql_search([SearchField, _, <<"nu">>])   -> {<<" AND ", SearchField/binary, " is null and 1 = ? ">>, [1]};
get_extra_sql_search([SearchField, _, <<"nn">>])   -> {<<" AND ", SearchField/binary, " is not null and 1 = ? ">>, [1]};
get_extra_sql_search([SearchField, V, <<"in">>])   -> 
    {Placeholders, VParams} = split_in_statement(V),
    lager:debug("IN: ~p ~p ~p", [V, Placeholders, VParams]),
    {<<" AND ", SearchField/binary, " in (", Placeholders/binary ,") ">>, VParams};
get_extra_sql_search([SearchField, V, <<"ni">>])   -> 
    {Placeholders, VParams} = split_in_statement(V),
    {<<" AND ", SearchField/binary, " not in (",Placeholders/binary ,") ">>, VParams};
get_extra_sql_search(_) -> {<<" AND 1 = ? ">>, [1]}.

-spec split_in_statement(binary()) -> {binary(), list()}.
split_in_statement(V) ->
   VParams = re:split(V, ","), 
   {list_to_binary(string:join(["?" || _ <- VParams], ",")), VParams}.

