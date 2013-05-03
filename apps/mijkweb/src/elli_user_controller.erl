-module(elli_user_controller).

-include("include/consts.hrl").
-include("include/mijkweb_consts.hrl").

-export([
    register_user/1,
    register_app_user/2,
    register_app_user/1,
    poll/2,
    push/2,
    user_profile/2,
    set_key/2,
    get_key/2,
    del_key/2,
    check_session/2
]).


register_app_user(ElliReq) ->
    lager:debug("Register app user (NA): ~p", [ElliReq]),
    try
        Request = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
        lager:debug("RAU0 ~p", [Request]),
        {Json}  = jiffy:decode(Request),
        lager:debug("RAU1", []),
        [Type, Login, Password, DevKey] = [proplists:get_value(X, Json, undefined) || 
            X <- [<<"type">>, <<"applogin">>, <<"apppassword">>, <<"developer_guid">>]],
        lager:debug("RE1: ~p, ~p, ~p", [Type, Login, Password]),
        T = if is_binary(Type) -> Type; true -> <<"unknown">> end,
        is_binary(Login) orelse throw({error, nologin, T}), 
        is_binary(Password) orelse throw({error, nopassword, T}),
        is_binary(DevKey) orelse throw({error, nodevkey, T}),

        [AppType, DeviceID] = mijkweb_utils:plist_values(Json, 
            [{<<"apptype">>, <<"WEBAPP">>}, 
             {<<"device">>,  <<"">>}]),
        is_binary(DeviceID) orelse throw({error, nodeviceid, T}),
        lists:member(AppType, [<<"WEBAPP">>, <<"IPHONE">>, <<"ANDROID">>]) orelse throw({error, baddevicetype}),

        Type =:= <<"createappuser">> orelse throw({error, badcommand, T}),
        lager:debug("RAU5", []),
        case model_service_user:createappuser(Login, Password, DevKey, AppType, DeviceID, []) of
            {ok, _}         -> 
               Response = {[
                    {<<"type">>, Type},
                    {<<"status">>, <<"ok">>}
               ]},
               {ok, [], mijkweb_response:json_ok_response(Response)};
            {error, Reason1} -> throw({error, Reason1, T})
        end
    catch
        throw:{error, nologin,      T1} -> {ok, [], mijkweb_response:json_error_response(?AUTH_NOLOGIN, T1)};
        throw:{error, nopassword,   T1} -> {ok, [], mijkweb_response:json_error_response(?AUTH_NOPASSWORD, T1)};
        throw:{error, badcommand,   T1} -> {ok, [], mijkweb_response:json_error_response(?BADCOMMAND, T1)};
        throw:{error, badlogintype, T1} -> {ok, [], mijkweb_response:json_error_response(?BADLOGINTYPE, T1)};
        throw:{error, nodevkey,     T1} -> {ok, [], mijkweb_response:json_error_response(?NODEVKEY, T1)};
        throw:{error, logininuse,   T1} -> {ok, [], mijkweb_response:json_error_response(?LOGININUSE, T1)};
        throw:{error, nodeviceid,   T1} -> {ok, [], mijkweb_response:json_error_response(?NODEVICEID, T1)};
        throw:{error, baddevicetype,T1} -> {ok, [], mijkweb_response:json_error_response(?BADDEVICETYPE, T1)};
        throw:{error, {_, invalid_json}}-> {ok, [], mijkweb_response:json_error_response(?INVALID_JSON)};
        Error:Reason                    -> 
                                     lager:error("register app user UE: ~p ~p ", [Error, Reason]),
                                     {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)} 
    end.

register_app_user(ElliReq, [_,_,SessionData]) ->
    lager:debug("Register app user: ~p ~p", [ElliReq, SessionData]),
    try
        Request = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
        lager:debug("RAU0 ~p", [Request]),
        {Json}  = jiffy:decode(Request),
        lager:debug("RAU1", []),
        [Type, Login, Password] = [proplists:get_value(X, Json, undefined) || 
            X <- [<<"type">>, <<"applogin">>, <<"apppassword">>]],
        lager:debug("RE1: ~p, ~p, ~p", [Type, Login, Password]),
        T = if is_binary(Type) -> Type; true -> <<"unknown">> end,
        is_binary(Login) orelse throw({error, nologin, T}), 
        is_binary(Password) orelse throw({error, nopassword, T}),
        Type =:= <<"createappuser">> orelse throw({error, badcommand, T}),

        [AppType, DeviceID] = mijkweb_utils:plist_values(Json, 
            [{<<"apptype">>, <<"WEBAPP">>}, 
             {<<"device">>,  <<"">>}]),
        is_binary(DeviceID) orelse throw({error, nodeviceid, T}),
        lists:member(AppType, [<<"WEBAPP">>, <<"IPHONE">>, <<"ANDROID">>]) orelse throw({error, baddevicetype}),
        
        proplists:get_value(<<"logintype">>, SessionData, 0) =:= 1 orelse throw({error, badlogintype, T}),
        SysAccountID = proplists:get_value(<<"accountid">>, SessionData, undefined),
        is_integer(SysAccountID) orelse throw({error, badsysaccountid, T}),
        lager:debug("RAU5", []),
        case model_service_user:createappuser(Login, Password, SysAccountID, AppType, DeviceID, []) of
            {ok, _}         -> 
               Response = {[
                    {<<"type">>, Type},
                    {<<"status">>, <<"ok">>}
               ]},
               {ok, [], mijkweb_response:json_ok_response(Response)};
            {error, Reason1} -> throw({error, Reason1, T})
        end
    catch
        throw:{error, nologin,      T1} -> {ok, [], mijkweb_response:json_error_response(?AUTH_NOLOGIN, T1)};
        throw:{error, nopassword,   T1} -> {ok, [], mijkweb_response:json_error_response(?AUTH_NOPASSWORD, T1)};
        throw:{error, badcommand,   T1} -> {ok, [], mijkweb_response:json_error_response(?BADCOMMAND, T1)};
        throw:{error, badlogintype, T1} -> {ok, [], mijkweb_response:json_error_response(?BADLOGINTYPE, T1)};
        throw:{error, badsysaccountid,T1} -> {ok, [], mijkweb_response:json_error_response(?BADSYSACCOUNT, T1)};
        throw:{error, logininuse,   T1} -> {ok, [], mijkweb_response:json_error_response(?LOGININUSE, T1)};
        throw:{error, nodeviceid,   T1} -> {ok, [], mijkweb_response:json_error_response(?NODEVICEID, T1)};
        throw:{error, baddevicetype,T1} -> {ok, [], mijkweb_response:json_error_response(?BADDEVICETYPE, T1)};
        throw:{error, {_, invalid_json}}-> {ok, [], mijkweb_response:json_error_response(?INVALID_JSON)};
        Error:Reason                    -> 
                                     lager:error("register app user UE: ~p ~p ", [Error, Reason]),
                                     {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)} 
    end.

register_user(ElliReq) ->
    lager:debug("Register user: ~p", [ElliReq]),
    try
        Request = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
        {Json}  = jiffy:decode(Request),
        [Type, Login, Password, Email] = [proplists:get_value(X, Json, undefined) || 
            X <- [<<"type">>, <<"login">>, <<"password">>, <<"email">>]],
        lager:debug("RE1: ~p, ~p, ~p, ~p", [Type, Login, Password, Email]),
        T = if is_binary(Type) -> Type; true -> <<"unknown">> end,
        is_binary(Login) orelse throw({error, nologin, T}), 
        is_binary(Password) orelse throw({error, nopassword, T}),
        Type =:= <<"createsysuser">> orelse throw({error, badcommand, T}),
        mijkweb_utils:is_email(Email) orelse throw({error, bademail, T}),
        lager:debug("ReUs ~p ~p", [Login, Password]),
        case model_service_user:create_user(Login, Password, Email) of
            {ok, _,_, WebMasterGuid} when is_binary(WebMasterGuid) ->
               Response = {[
                    {<<"type">>, Type},
                    {<<"status">>, <<"ok">>},
                    {<<"developer_guid">>, WebMasterGuid}
               ]},
               {ok, [], mijkweb_response:json_ok_response(Response)};
            {error, Reason1} -> throw({error, Reason1, T})
        end
    catch
        throw:{error, nologin,      T1} -> {ok, [], mijkweb_response:json_error_response(?AUTH_NOLOGIN, T1)};
        throw:{error, nopassword,   T1} -> {ok, [], mijkweb_response:json_error_response(?AUTH_NOPASSWORD, T1)};
        throw:{error, badcommand,   T1} -> {ok, [], mijkweb_response:json_error_response(?BADCOMMAND, T1)};
        throw:{error, bademail,     T1} -> {ok, [], mijkweb_response:json_error_response(?BADEMAIL, T1)};
        throw:{error, logininuse,   T1} -> {ok, [], mijkweb_response:json_error_response(?LOGININUSE, T1)};
        Error:Reason                    -> 
                                     lager:error("register user: ~p ~p ", [Error, Reason]),
                                     {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)} 
    end.

poll(ElliReq, [SSID, [_CookieHeader], SessionData]) ->
    lists:member(proplists:get_value(<<"logintype">>, SessionData, 0), [1,2]) orelse throw({error, badlogintype, <<"poll">>}),
    Req = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
    long_polling:poll_request(Req, SSID, SessionData).

push(ElliReq, [_SSID, [_CookieHeader], SessionData]) ->
    lager:debug("EUC PUSH1", []),
    lists:member(proplists:get_value(<<"logintype">>, SessionData, 0), [1,2]) orelse throw({error, badlogintype, <<"push">>}),
    lager:debug("EUC PUSH2", []),
    Req = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
    lager:debug("EUC PUSH3", []),
    long_polling:push_request(Req, SessionData).

check_session(ElliReq, [_SSID, [_CookieHeader], SessionData]) ->
    try
        Request = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
        {Json}  = jiffy:decode(Request),
        Type = proplists:get_value(<<"type">>, Json, undefined),
        T = if is_binary(Type) -> Type; true -> <<"unknown">> end,
        Type =:= <<"check_session">> orelse throw({error, badcommand, T}),
        SysAccountID = proplists:get_value(<<"accountid">>, SessionData, undefined),
        is_integer(SysAccountID) orelse throw({error, badsysaccountid, T}),
        LoginType = proplists:get_value(<<"logintype">>, SessionData, 0),
        Response = {[
            {<<"type">>, Type},
            {<<"status">>, <<"ok">>},
            {<<"login_type">>, LoginType},
            {<<"sysaccid">>, SysAccountID}
        ]},
        lager:debug("CS ok resp: ~p ", [Response]),
        {ok, [], mijkweb_response:json_ok_response(Response)}
    catch
        throw:{error, badsysaccountid,T1} -> {ok, [], mijkweb_response:json_error_response(?BADSYSACCOUNT, T1)};
        throw:{error, badcommand,   T1}   -> {ok, [], mijkweb_response:json_error_response(?BADCOMMAND, T1)};
        throw:{error, badprofile,   T1}   -> {ok, [], mijkweb_response:json_error_response(?BADPROFILE, T1)};
        throw:{error, badlogintype, T1}   -> {ok, [], mijkweb_response:json_error_response(?BADLOGINTYPE, T1)};
        Error:Reason                      -> 
                                     lager:error("register app user UE: ~p ~p ", [Error, Reason]),
                                     {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)} 
    end.
    

user_profile(ElliReq, [_SSID, [_CookieHeader], SessionData]) ->
    try
        Request = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
        {Json}  = jiffy:decode(Request),
        Type = proplists:get_value(<<"type">>, Json, undefined),
        T = if is_binary(Type) -> Type; true -> <<"unknown">> end,
        Type =:= <<"get_profile">> orelse throw({error, badcommand, T}),
        SysAccountID = proplists:get_value(<<"accountid">>, SessionData, undefined),
        is_integer(SysAccountID) orelse throw({error, badsysaccountid, T}),
        proplists:get_value(<<"logintype">>, SessionData, 0) =:= 1 orelse throw({error, badlogintype, T}),
        case model_service_user:get_user_profile(SysAccountID) of
            {ok, Profile} -> 
               Response = {[
                    {<<"type">>, Type},
                    {<<"status">>, <<"ok">>},
                    {<<"data">>, Profile}
               ]},
               lager:debug("UP ok resp: ~p ", [Response]),
               {ok, [], mijkweb_response:json_ok_response(Response)}
            ;_            -> throw({error, badprofile, T})
        end
    catch
        throw:{error, badsysaccountid,T1} -> {ok, [], mijkweb_response:json_error_response(?BADSYSACCOUNT, T1)};
        throw:{error, badcommand,   T1}   -> {ok, [], mijkweb_response:json_error_response(?BADCOMMAND, T1)};
        throw:{error, badprofile,   T1}   -> {ok, [], mijkweb_response:json_error_response(?BADPROFILE, T1)};
        throw:{error, badlogintype, T1}   -> {ok, [], mijkweb_response:json_error_response(?BADLOGINTYPE, T1)};
        Error:Reason                      -> 
                                     lager:error("register app user UE: ~p ~p ", [Error, Reason]),
                                     {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)} 
    end.

set_key(ElliReq, [_SSID, [_CookieHeader], SessionData]) ->
    try
        Request = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
        {Json}  = jiffy:decode(Request),
        [Type, Key, Value] = mijkweb_utils:plist_values(Json, 
            [{<<"type">>, undefined}, 
             {<<"key">>,  undefined}, 
             {<<"value">>, undefined}]),
        T = if is_binary(Type) -> Type; true -> <<"unknown">> end,
        Type =:= <<"set_key">> orelse throw({error, badcommand, T}),
        is_binary(Key) orelse throw({error, badcommand, T}),
        is_binary(Value) orelse throw({error, badcommand, T}),
        SysAccountID = case proplists:get_value(<<"logintype">>, SessionData, 0) of
            1 -> proplists:get_value(<<"accountid">>, SessionData, undefined);
            2 -> proplists:get_value(<<"loginextra">>, SessionData, undefined);
            _ -> throw({error, badlogintype, T})
        end,
        is_integer(SysAccountID) orelse throw({error, badsysaccountid, T}),
        case model_service_user:set_key(SysAccountID, Key, Value) of
            ok -> mijkweb_response:json_ok_response({[{<<"type">>, Type},{<<"status">>, <<"ok">>}]})
            ;_ -> throw({error, list_to_binary(io_lib:format("cannot set the key ~p , ~p , ~p", 
                                                                [SysAccountID, Key, Value])), T })
        end
    catch
        throw:{error, badcommand,   T1}   -> {ok, [], mijkweb_response:json_error_response(?BADCOMMAND, T1)};
        throw:{error, badlogintype, T1}   -> {ok, [], mijkweb_response:json_error_response(?BADLOGINTYPE, T1)};
        Error:Reason                      -> 
                                     lager:error("register app user UE: ~p ~p ", [Error, Reason]),
                                     {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)} 
    end.

get_key(ElliReq, [_SSID, [_CookieHeader], SessionData]) ->
    try
        Request = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
        {Json}  = jiffy:decode(Request),
        [Type, Key] = mijkweb_utils:plist_values(Json, 
            [{<<"type">>, undefined}, 
             {<<"key">>,  undefined}]),
        T = if is_binary(Type) -> Type; true -> <<"unknown">> end,
        Type =:= <<"get_key">> orelse throw({error, badcommand, T}),
        is_binary(Key) orelse throw({error, badcommand, T}),
        SysAccountID = case proplists:get_value(<<"logintype">>, SessionData, 0) of
            1 -> proplists:get_value(<<"accountid">>, SessionData, undefined);
            2 -> proplists:get_value(<<"loginextra">>, SessionData, undefined);
            _ -> throw({error, badlogintype, T})
        end,
        is_integer(SysAccountID) orelse throw({error, badsysaccountid, T}),
        case model_service_user:get_key(SysAccountID, Key) of
            {ok, Value} -> mijkweb_response:json_ok_response(
                            {[{<<"type">>, Type},{<<"status">>, <<"ok">>},{<<"value">>, Value}]})
            ;{error, <<"notfound">>} -> mijkweb_response:json_ok_response(
                            {[{<<"type">>, Type},{<<"status">>, <<"error">>},{<<"description">>, <<"key not found">>}]})
            ;_ -> throw({error, list_to_binary(io_lib:format("cannot get the key ~p , ~p", 
                                                                [SysAccountID, Key])), T })
        end
    catch
        throw:{error, badcommand,   T1}   -> {ok, [], mijkweb_response:json_error_response(?BADCOMMAND, T1)};
        throw:{error, badlogintype, T1}   -> {ok, [], mijkweb_response:json_error_response(?BADLOGINTYPE, T1)};
        Error:Reason                      -> 
                                     lager:error("register app user UE: ~p ~p ", [Error, Reason]),
                                     {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)} 
    end.

del_key(ElliReq, [_SSID, [_CookieHeader], SessionData]) ->
    try
        Request = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
        {Json}  = jiffy:decode(Request),
        [Type, Key] = mijkweb_utils:plist_values(Json, 
            [{<<"type">>, undefined}, 
             {<<"key">>,  undefined}]),
        T = if is_binary(Type) -> Type; true -> <<"unknown">> end,
        Type =:= <<"del_key">> orelse throw({error, badcommand, T}),
        is_binary(Key) orelse throw({error, badcommand, T}),
        SysAccountID = case proplists:get_value(<<"logintype">>, SessionData, 0) of
            1 -> proplists:get_value(<<"accountid">>, SessionData, undefined);
            2 -> proplists:get_value(<<"loginextra">>, SessionData, undefined);
            _ -> throw({error, badlogintype, T})
        end,
        is_integer(SysAccountID) orelse throw({error, badsysaccountid, T}),
        case model_service_user:del_key(SysAccountID, Key) of
            ok -> mijkweb_response:json_ok_response(
                            {[{<<"type">>, Type},{<<"status">>, <<"ok">>}]})
            ;_ -> throw({error, list_to_binary(io_lib:format("cannot delete the key ~p , ~p", 
                                                                [SysAccountID, Key])), T })
        end
    catch
        throw:{error, badcommand,   T1}   -> {ok, [], mijkweb_response:json_error_response(?BADCOMMAND, T1)};
        throw:{error, badlogintype, T1}   -> {ok, [], mijkweb_response:json_error_response(?BADLOGINTYPE, T1)};
        Error:Reason                      -> 
                                     lager:error("register app user UE: ~p ~p ", [Error, Reason]),
                                     {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)} 
    end.
