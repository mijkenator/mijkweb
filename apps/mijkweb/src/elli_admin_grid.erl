-module(elli_admin_grid).

-include("include/consts.hrl").
-include("include/mijkweb_consts.hrl").

-export([
    handler/2
]).

-define(FIELDS, [<<"id">>, <<"active">>, <<"login">>, <<"password">>, 
                 <<"remote_ip">>, <<"register_time">>, <<"last_login_time">>]).
-define(OPS,  [<<"eq">>,<<"ne">>,<<"bw">>,<<"bn">>,<<"ew">>,<<"en">>,
               <<"cn">>,<<"nc">>,<<"nu">>,<<"nn">>,<<"in">>,<<"ni">>]).
-define(SORT, [<<"asc">>, <<"desc">>]).

handler(ElliReq, [SSID, [CookieHeader], SessionData]) ->
   try 
        lager:debug("admin grid handler start", []),
        Type = cowboy_http:urldecode(elli_request:post_arg(<<"type">>, ElliReq, 
            elli_request:post_arg(<<"oper">>, ElliReq, undefined))),
        is_binary(Type) orelse throw({error, badcommand}),

        proplists:get_value(<<"logintype">>, SessionData, 0) =:= 1 orelse throw({error, badlogintype}),
        SysAccountID = proplists:get_value(<<"accountid">>, SessionData, undefined),    
        is_integer(SysAccountID) orelse throw({error, badsysaccountid}),

        action(Type, ElliReq, [SSID, [CookieHeader], SessionData, SysAccountID])
    catch
        throw:{error, badlogintype, T1} -> {ok, [], mijkweb_response:json_error_response(?BADLOGINTYPE, T1)};
        throw:{error, badcommand, T1} -> {ok, [], mijkweb_response:json_error_response(?BADCOMMAND, T1)};
        Error:Reason -> 
             lager:error("Unexpected error: ~p ~p ~p ", [?MODULE, Error, Reason]),
             {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)} 
    end.

action(<<"select">>, ElliReq, [_SSID, [CookieHeader], _SessionData, SysAccountID]) ->
    Rows = list_to_integer(binary_to_list(cowboy_http:urldecode(elli_request:post_arg(<<"rows">>, ElliReq, <<"10">>)))),
    Page = list_to_integer(binary_to_list(cowboy_http:urldecode(elli_request:post_arg(<<"page">>, ElliReq, 1)))),
    SIdx = cowboy_http:urldecode(elli_request:post_arg(<<"sidx">>, ElliReq, <<>>)),
    size(SIdx) > 0 orelse throw({error, badcommand}),
    SOrd = cowboy_http:urldecode(elli_request:post_arg(<<"sord">>, ElliReq, <<>>)),
    size(SOrd) > 0 orelse throw({error, badcommand}),
   
    lists:member(SIdx, ?FIELDS) orelse throw({error, badcommand}),
    lists:member(SOrd, ?SORT)   orelse throw({error, badcommand}),
    lager:debug("EAGS1 ~p ~p", [?FIELDS, ?SORT]),

    SearchField  = cowboy_http:urldecode(elli_request:post_arg(<<"searchField">>,  ElliReq, <<"">>)),
    SearchString = cowboy_http:urldecode(elli_request:post_arg(<<"searchString">>, ElliReq, <<"">>)),
    SearchOper   = cowboy_http:urldecode(elli_request:post_arg(<<"searchOper">>,   ElliReq, <<"">>)),
    lager:debug("EAGS2 ~p", [{SearchField, SearchOper, SearchString}]),
    case {is_binary(SearchField), SearchField} of
        {true, BSF } when size(BSF) > 0 -> 
            (is_binary(SearchOper) andalso is_binary(SearchOper)) orelse throw({error, badcommand, <<"select">>}),
            lists:member(SearchField, ?FIELDS) orelse throw({error, badcommand}),
            lists:member(SearchOper,  ?OPS)    orelse throw({error, badcommand});
        _ -> ok
    end,
    lager:debug("EAGS3", []),

    {Total, Data} = model_admin_user_grid:select(SysAccountID, Rows, Page, SIdx, SOrd, 
        [{search, [SearchField, SearchString, SearchOper]}]), 
    lager:debug("EAGS4 ~p ~p", [Total, Data]),
    ExtraP = case round(Total/Rows) * Rows < Total of
        true -> 1;
        _    -> 0
    end,
    TotalPages = round(Total/Rows) + ExtraP,
    Response = {[
        {<<"page">>, Page},
        {<<"total">>, TotalPages},
        {<<"records">>, Total},
        {<<"rows">>, [{[{<<"id">>, ID},{<<"cell">>, 
            [ID|lists:map(fun({datetime, {Date,Time}})-> 
                    mijkweb_utils:format_time({Date, Time});(E)->E end,TL)]}]} || [ID|TL] <- Data]}
    ]},
    {ok,[CookieHeader], mijkweb_response:json_ok_response(Response)};

action(<<"add">>, ElliReq, [_SSID, [CookieHeader], _SessionData, SysAccountID]) ->
    Login    = cowboy_http:urldecode(elli_request:post_arg(<<"login">>, ElliReq, undefined)),
    Password = cowboy_http:urldecode(elli_request:post_arg(<<"password">>, ElliReq, undefined)),
    (is_binary(Login) orelse Login=:=<<"">>) orelse throw({error, badcommand, <<"add_app_user">>}),
    (is_binary(Password) orelse Password=:=<<"">>) orelse throw({error, badcommand, <<"add_app_user">>}),
    IPAddr = <<"0.0.0.0">>,
    case model_service_user:createappuser(Login, Password, SysAccountID, <<"WEBAPP">>, <<"">>, [{<<"ipaddr">>, IPAddr}]) of
        {ok, _}         -> 
           Response = {[
                {<<"oper">>, <<"add">>},
                {<<"status">>, <<"ok">>}
           ]},
           {ok, [CookieHeader], mijkweb_response:json_ok_response(Response)};
        {error, Reason} -> throw({error, Reason, <<"add_app_user">>})
    end;

action(<<"edit">>, ElliReq, [_SSID, [CookieHeader], _SessionData, SysAccountID]) ->
    lager:debug("admin grid edit action start", []),
    Login    = cowboy_http:urldecode(elli_request:post_arg(<<"login">>, ElliReq, undefined)),
    Password = cowboy_http:urldecode(elli_request:post_arg(<<"password">>, ElliReq, undefined)),
    (is_binary(Login) orelse Login=:=<<"">>) orelse throw({error, badcommand, <<"edit_app_user">>}),
    (is_binary(Password) orelse Password=:=<<"">>) orelse throw({error, badcommand, <<"edit_app_user">>}),
    Id = list_to_integer(binary_to_list(cowboy_http:urldecode(elli_request:post_arg(<<"id">>, ElliReq, <<"0">>)))),
    Active = list_to_integer(binary_to_list(cowboy_http:urldecode(elli_request:post_arg(<<"active">>, ElliReq, <<"1">>)))),
    lager:debug("admin grid edit action params: ~p ~p ~p ~p", [Login, Password, Id, Active]),
    case model_service_user:updateappuser(Login, Password, Active, SysAccountID, Id) of
        {ok, _}         -> 
           Response = {[
                {<<"oper">>, <<"edit">>},
                {<<"status">>, <<"ok">>}
           ]},
           {ok, [CookieHeader], mijkweb_response:json_ok_response(Response)};
        {error, Reason} -> throw({error, Reason, <<"edit_app_user">>})
    end;

action(<<"del">>, ElliReq, [_SSID, [CookieHeader], _SessionData, SysAccountID]) ->
    Id = list_to_integer(binary_to_list(cowboy_http:urldecode(elli_request:post_arg(<<"id">>, ElliReq, <<"0">>)))),
    case model_service_user:deleteappuser(SysAccountID, Id) of
        {ok, _}         -> 
           Response = {[
                {<<"oper">>, <<"deletet">>},
                {<<"status">>, <<"ok">>}
           ]},
           {ok, [CookieHeader], mijkweb_response:json_ok_response(Response)};
        {error, Reason} -> throw({error, Reason, <<"delete_app_user">>})
    end;

action(Type, _, [_, CH, _]) -> {ok, CH, mijkweb_response:json_error_response(?UNKNOWNCOMMAND, Type)}.


