-module(elli_admin_user_controller).

-include("include/consts.hrl").
-include("include/mijkweb_consts.hrl").

-export([
    handler/2
]).

handler(ElliReq, [SSID, [CookieHeader], SessionData]) ->
   try 
        lager:debug("EAUCH1 ~p", [ElliReq]),
        Request = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
        lager:debug("EAUCH2 ~p", [Request]),
        {Json}  = jiffy:decode(Request),
        [Type] = mijkweb_utils:plist_values(Json, [{<<"type">>, undefined}]),
        T = if is_binary(Type) -> Type; true -> <<"unknown">> end,

        proplists:get_value(<<"logintype">>, SessionData, 0) =:= 1 orelse throw({error, badlogintype, T}),
        SysAccountID = proplists:get_value(<<"accountid">>, SessionData, undefined),
        is_integer(SysAccountID) orelse throw({error, badsysaccountid, T}),

        action(Type, Json, ElliReq, [SSID, [CookieHeader], SessionData, SysAccountID])
    catch
        throw:{error, badlogintype, T1} -> {ok, [], mijkweb_response:json_error_response(?BADLOGINTYPE, T1)};
        Error:Reason -> 
             lager:error("Unexpected error: ~p ~p ~p ", [?MODULE, Error, Reason]),
             {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)} 
    end.

action(<<"create">>, _Json, ElliReq, [SSID, [CookieHeader], SessionData, _]) ->
    %
    % just call previous code, todo: refactor 
    %
    elli_user_controller:register_app_user(ElliReq, [SSID, [CookieHeader], SessionData]);
action(<<"delete">>, Json, ElliReq, [_SSID, [_CookieHeader], SessionData]) -> ok;
action(<<"update">>, Json, ElliReq, [_SSID, [_CookieHeader], SessionData]) -> ok;
action(<<"list">>,   Json, ElliReq, [_SSID, [_CookieHeader], SessionData, SysAccountID]) ->
    model_service_user:app_users_list(SysAccountID);
action(<<"channels-list">>,   Json, _ElliReq, [_SSID, [CookieHeader], _SessionData, SysAccountID]) ->
    lager:debug("CHL1 ~p", [Json]),
    [Str, Limit] = mijkweb_utils:plist_values(Json, [{<<"str">>, <<"%">>},{<<"limit">>, 10}]),
    lager:debug("CHL2 ->'~p'<- ~p", [Str, Limit]),
    is_binary(Str) orelse throw({error, badcommand, <<"channels-list">>}),
    Ret = model_service_user:channels_list(SysAccountID, Str, Limit),
    lager:debug("CHL3 ~p", [Ret]),
    Response = {[
        {<<"type">>, <<"channels-list">>},
        {<<"status">>, <<"ok">>},
        {<<"data">>, Ret}
    ]},
    lager:debug("CHLIST resp: ~p ", [Response]),
    {ok, [CookieHeader], mijkweb_response:json_ok_response(Response)};
action(<<"get-admin-stats">>, _Json, _ElliReq, [_SSID, [CookieHeader], _SessionData, _SysAccountID]) ->
    Ret = [
        {<<"online">>, 11},
        {<<"reg_today">>, 11},
        {<<"logins_today">>, 11},
        {<<"events_today">>, 11},
        {<<"key_using">>, 11}
    ],
    Response = {[
        {<<"type">>, <<"get-admin-stats">>},
        {<<"status">>, <<"ok">>},
        {<<"data">>, {Ret}}
    ]},
    {ok, [CookieHeader], mijkweb_response:json_ok_response(Response)};
action(Type, _, _, [_, CH, _]) -> {ok, CH, mijkweb_response:json_error_response(?UNKNOWNCOMMAND, Type)}.
