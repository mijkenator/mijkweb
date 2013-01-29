-module(elli_user_controller).

-include("include/consts.hrl").
-include("include/mijkweb_consts.hrl").

-export([
    register_user/1,
    register_app_user/2,
    register_app_user/1,
    poll/2,
    push/2
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
        Type =:= <<"createappuser">> orelse throw({error, badcommand, T}),
        lager:debug("RAU5", []),
        case model_service_user:createappuser(Login, Password, DevKey) of
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
        proplists:get_value(<<"logintype">>, SessionData, 0) =:= 1 orelse throw({error, badlogintype, T}),
        SysAccountID = proplists:get_value(<<"accountid">>, SessionData, undefined),
        is_integer(SysAccountID) orelse throw({error, badsysaccountid, T}),
        lager:debug("RAU5", []),
        case model_service_user:createappuser(Login, Password, SysAccountID) of
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
    proplists:get_value(<<"logintype">>, SessionData, 0) =:= 2 orelse throw({error, badlogintype, <<"poll">>}),
    Req = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
    long_polling:poll_request(Req, SSID, SessionData).

push(ElliReq, [_SSID, [_CookieHeader], SessionData]) ->
    lager:debug("EUC PUSH1", []),
    proplists:get_value(<<"logintype">>, SessionData, 0) =:= 2 orelse throw({error, badlogintype, <<"push">>}),
    lager:debug("EUC PUSH2", []),
    Req = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, ElliReq, <<"{}">>)),
    lager:debug("EUC PUSH3", []),
    long_polling:push_request(Req, SessionData).
