-module(elli_admin_appuserdetails).

-include("include/consts.hrl").
-include("include/mijkweb_consts.hrl").

-export([
    handler/2
]).


-define(FIELDS, [<<"id">>, <<"appuserid">>, <<"apptype">>, <<"deviceid">>]).
-define(OPS,  [<<"eq">>,<<"ne">>,<<"bw">>,<<"bn">>,<<"ew">>,<<"en">>,
               <<"cn">>,<<"nc">>,<<"nu">>,<<"nn">>,<<"in">>,<<"ni">>]).
-define(SORT, [<<"asc">>, <<"desc">>]).


handler(ElliReq, [SSID, [CookieHeader], SessionData]) ->
   try 
        lager:debug("admin appuserdetails handler start", []),
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
    AppUserID = list_to_integer(binary_to_list(cowboy_http:urldecode(elli_request:post_arg(<<"id">>, ElliReq, <<"0">>)))),
    SIdx = cowboy_http:urldecode(elli_request:post_arg(<<"sidx">>, ElliReq, <<>>)),
    size(SIdx) > 0 orelse throw({error, badcommand}),
    SOrd = cowboy_http:urldecode(elli_request:post_arg(<<"sord">>, ElliReq, <<>>)),
    size(SOrd) > 0 orelse throw({error, badcommand}),
    lager:debug("EAAS0",[]),
   
    lists:member(SIdx, ?FIELDS) orelse throw({error, badcommand}),
    lists:member(SOrd, ?SORT)   orelse throw({error, badcommand}),
    lager:debug("EAAS1 ~p ~p", [?FIELDS, ?SORT]),

    SearchField  = cowboy_http:urldecode(elli_request:post_arg(<<"searchField">>,  ElliReq, <<"">>)),
    SearchString = cowboy_http:urldecode(elli_request:post_arg(<<"searchString">>, ElliReq, <<"">>)),
    SearchOper   = cowboy_http:urldecode(elli_request:post_arg(<<"searchOper">>,   ElliReq, <<"">>)),
    lager:debug("EAAS2 ~p", [{SearchField, SearchOper, SearchString}]),
    case {is_binary(SearchField), SearchField} of
        {true, BSF } when size(BSF) > 0 -> 
            (is_binary(SearchOper) andalso is_binary(SearchOper)) orelse throw({error, badcommand, <<"select">>}),
            lists:member(SearchField, ?FIELDS) orelse throw({error, badcommand}),
            lists:member(SearchOper,  ?OPS)    orelse throw({error, badcommand});
        _ -> ok
    end,
    lager:debug("EAAS3", []),

    {Total, Data} = model_admin_user_grid:select_appuserdetails(SysAccountID, Rows, Page, SIdx, SOrd, 
        [{search, [SearchField, SearchString, SearchOper]},
         {<<"appuserid">>, AppUserID}
        ]), 
    lager:debug("EAAS4 ~p ~p", [Total, Data]),
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
action(Type, _, [_, CH, _]) -> {ok, CH, mijkweb_response:json_error_response(?UNKNOWNCOMMAND, Type)}.
