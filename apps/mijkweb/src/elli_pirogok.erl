-module(elli_pirogok).
-export([handle/2, handle_event/3]).

-include("include/consts.hrl").
-include("include/mijkweb_consts.hrl").
-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('POST', [<<"auth">>], Req) ->
    lager:debug("EP: 2", []),
    try
        Request = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, Req, <<"{}">>)),
        {Json}  = jiffy:decode(Request),
        [Type, Login, Password] = [proplists:get_value(X, Json, undefined) || X <- [<<"type">>, <<"login">>, <<"password">>]],
        T = if is_binary(Type) -> Type; true -> <<"unknown">> end,
        is_binary(Login) orelse throw({error, nologin, T}), 
        is_binary(Password) orelse throw({error, nopassword, T}),
        Type =:= <<"login">> orelse throw({error, badcommand, T}),
        lager:debug("EP: 2-4 ~p ~p", [Login, Password]),
        case auth_utils:check_credentials(Login, Password) of
            {ok, {AccountId, LoginType, LoginExtra}} ->
                       lager:debug("EP: 2-5", []),
                       [_, RespHeaders] = mijkweb_session:session_process({ellimcd, <<"auth">>}, Req, 
                       [{<<"status">>, <<"authorized">>}, 
                        {<<"logintype">>, LoginType},
                        {<<"timestamp">>, mijkweb_utils:now()},
                        {<<"loginextra">>, LoginExtra},
                        {<<"accountid">>, AccountId}]),
                       Response = {[
                            {<<"type">>, Type},
                            {<<"status">>, <<"ok">>},
                            {<<"ut">>, LoginType}
                       ]},
                       lager:debug("EP: 2-6 ~p ", [Response]),
                       mijk_statist:inc_logins_today(if LoginType=:=1->AccountId;true->LoginExtra end, 1),
                       mijk_statist:inc_events_today(if LoginType=:=1->AccountId;true->LoginExtra end, 1),
                       {ok, RespHeaders, mijkweb_response:json_ok_response(Response)}
            ;_E      ->
                lager:error("AUCC ~p ~p", [_E, ?AUTH_FAILED]),
                {ok, [], mijkweb_response:json_error_response(?AUTH_FAILED, T)}
        end
    catch
        throw:{error, nologin, T1}    -> {ok, [], mijkweb_response:json_error_response(?AUTH_NOLOGIN, T1)};
        throw:{error, nopassword, T1} -> {ok, [], mijkweb_response:json_error_response(?AUTH_NOPASSWORD, T1)};
        throw:{error, badcommand, T1} -> {ok, [], mijkweb_response:json_error_response(?BADCOMMAND, T1)};
        Error:Reason                  -> 
                                     lager:error("EP AUTH error:~p ~p ", [Error, Reason]),
                                     {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)} 
    end;

handle('POST', [<<"logout">>], Req) ->
    lager:debug("pirogok logout", []),
    CookieHeader = mijkweb_session:clean_session(ellimcd, Req),
    lager:debug("pirogok logout cs: ~p", [CookieHeader]),
    {ok, [CookieHeader], <<"{\"type\":\"logout\",\"status\":\"ok\"}">>};

handle(ReqMethod, [<<"auth">>|_] = Path, Req) ->
    lager:debug("EP: 1", []),
    case mijkweb_session:check_auth(ellimcd, Req) of
        {ok, SessionData} -> auth_handle(ReqMethod, Path, Req, SessionData)
        ;_R               -> 
            lager:debug("handler to auth--->: ~p", [_R]), 
            {ok, [], mijkweb_response:json_error_response(?AUTH_REQUIRED)}
    end;

handle(ReqMethod, Path, Req) ->
    lager:error("unknown regular method: ~p ~p ~p", [ReqMethod, Path, Req]),
    case mijkweb_dispatch:get_route(ReqMethod, Path) of
        {ok, Module, Method} -> erlang:apply(Module, Method, [Req])
        ;_ -> {404, [], <<"Ты кто такой, давай досвидания!">>}
    end.

auth_handle('POST', [<<"auth">>, <<"testc">>, <<"testm">>], _Req, SessionData) ->
    [_SSID, [CookieHeader], _SessionData] = SessionData,
    {ok, [CookieHeader], <<"auth testc testm">>};

auth_handle(ReqMethod, Path, Req, [_, Cookies, _SSData] = SessionData) ->
    lager:debug("AH unknown auth method -> go to dispatch: ~p ~p ~p ~p", [ReqMethod, Path, Req, SessionData]),
    case mijkweb_dispatch:get_route(ReqMethod, Path) of
        {ok, Module, Method} ->
            lager:debug("auth dispatch to ~p ~p", [Module, Method]),
            case erlang:apply(Module, Method, [Req, SessionData]) of
                {ok, [], Resp} -> {ok, Cookies, Resp};
                {ok, He, Resp} -> {ok, He, Resp}
                %;R when is_binary(R) -> {ok, [], R}
                ;R when is_binary(R) -> {ok, Cookies, R}
                ;R ->
                    lager:debug("AH nok resp: ~p", [R]),
                    R
            end
        ;_ -> {404, [], <<"Ты кто такой, давай досвидания!">>}
    end.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) -> ok.

