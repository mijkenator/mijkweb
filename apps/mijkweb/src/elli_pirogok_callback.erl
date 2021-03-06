-module(elli_pirogok_callback).
-export([handle/2, handle_event/3]).

-include("include/consts.hrl").
-include("include/mijkweb_consts.hrl").
-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"test">>, <<"elli">>], _Req) ->
    %% Reply with a normal response. 'ok' can be used instead of '200'
    %% to signal success.
    {ok, [], <<"Ты кто такой, давай техзадание!">>};

handle('GET',[<<"test">>, <<"elli">>, <<"cookie">>], Req) ->
    case elli_request:get_header(<<"Cookie">>, Req, undefined) of
        undefined -> lager:debug("E:NC", []), {ok, mcache_get_session_header(undefined), <<"No cookie \n">>};
        Cookies   -> 
            lager:debug("E:C: ~p ~n", [Cookies]),
            {RespHeaders, RespBody} = case [{Key, Value} || {Key, Value} <- 
                                            tests:lt_parse_cookie(Cookies), Key =:= <<"MIJKSSID">> ] of
                []          ->
                    {mcache_get_session_header(undefined), list_to_binary("Cookie:" ++ io_lib:format("~p",[Cookies]))};
                [{_, SSID}] ->
                    lager:debug("E:C:SSID ~p",[SSID]),
                    IpS = list_to_binary(io_lib:format("~p", [inet:getifaddrs()])),
                    {mcache_get_session_header(SSID), <<"Kolobok alive!\n<br>", IpS/binary>>}
            end,
            {ok, RespHeaders, RespBody}
    end;

handle('GET',[<<"test">>, <<"elli">>, <<"cookie">>, <<"mcd">>], Req) ->
    case elli_request:get_header(<<"Cookie">>, Req, undefined) of
        undefined -> lager:debug("E:NC", []), {ok, mcd_get_session_header(undefined), <<"No cookie \n">>};
        Cookies   -> 
            lager:debug("E:C: ~p ~n", [Cookies]),
            {RespHeaders, RespBody} = case [{Key, Value} || {Key, Value} <- 
                                            tests:lt_parse_cookie(Cookies), Key =:= <<"MIJKSSID">> ] of
                []          ->
                    {mcd_get_session_header(undefined), list_to_binary("Cookie:" ++ io_lib:format("~p",[Cookies]))};
                [{_, SSID}] ->
                    lager:debug("E:C:SSID ~p",[SSID]),
                    IpS = list_to_binary(io_lib:format("~p", [inet:getifaddrs()])),
                    {mcd_get_session_header(SSID), <<"Kolobok alive!\n<br>", IpS/binary>>}
            end,
            {ok, RespHeaders, RespBody}
    end;

handle('POST', [<<"poll">>], Req) ->
    io:format("--> ~p ~n", [Req]),
    try
        [SSID, RespHeaders] = mijkweb_session:session_process(elli, Req),
        lager:debug("ElliPoll: ~p ~p",[SSID, RespHeaders]),
        {ok, RespHeaders,               
         long_polling:poll_request(cowboy_http:urldecode(elli_request:post_arg(<<"request">>, Req, <<"{}">>)),SSID)}
    catch
        Error:Reason ->
            lager:error("elli poll error: ~p ~p", [Error, Reason]),
            {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)}
    end;

handle('POST', [<<"push">>], Req) ->
    try
        [SSID, RespHeaders] = mijkweb_session:session_process(elli, Req),
        lager:debug("ElliPush: ~p ~p",[SSID, RespHeaders]),
        {ok, RespHeaders,               
         long_polling:push_request(cowboy_http:urldecode(elli_request:post_arg(<<"request">>, Req, <<"{}">>)))}
    catch
        Error:Reason ->
            lager:error("elli push error: ~p ~p", [Error, Reason]),
            {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)}
    end;

handle(_, _, _Req) ->
    {404, [], <<"Ты кто такой, давай досвидания!">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) -> ok.

mcache_get_session_header(SessionCookie) ->
    case SessionCookie of
        undefined ->
            [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:mcache_create_session(1),[{max_age, ?SESSION_AGE},{path, <<"/">>}])];
        SSID      ->
            case mijk_session:mcache_check_session_data(SSID) of
                {ok, Data} when is_binary(Data) ->
                    EData = mijk_session:mysql_to_erl(Data),
                    NewData = case proplists:get_value(<<"counter">>, EData, undefined) of
                        undefined            -> [{<<"counter">>, 1}] ++ EData;
                        V when is_integer(V) -> lists:keyreplace(<<"counter">>, 1, EData, {<<"counter">>, V+1})
                    end,
                    mijk_session:mcache_update_session(SSID, NewData),
                    [cowboy_cookies:cookie(<<"MIJKSSID">>, SSID,[{max_age, ?SESSION_AGE},{path, <<"/">>}])]
                ;_ ->
                    [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:mcache_create_session(1),[{max_age, ?SESSION_AGE},{path, <<"/">>}])]
            end
    end.

mcd_get_session_header(SessionCookie) ->
    case SessionCookie of
        undefined ->
            [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:mcd_create_session(1),[{max_age, ?SESSION_AGE},{path, <<"/">>}])];
        SSID      ->
            case mijk_session:mcd_check_session_data(SSID) of
                {ok, Data} when is_binary(Data) ->
                    EData = mijk_session:mysql_to_erl(Data),
                    NewData = case proplists:get_value(<<"counter">>, EData, undefined) of
                        undefined            -> [{<<"counter">>, 1}] ++ EData;
                        V when is_integer(V) -> lists:keyreplace(<<"counter">>, 1, EData, {<<"counter">>, V+1})
                    end,
                    mijk_session:mcd_update_session(SSID, NewData),
                    [cowboy_cookies:cookie(<<"MIJKSSID">>, SSID,[{max_age, ?SESSION_AGE},{path, <<"/">>}])]
                ;_ ->
                    [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:mcd_create_session(1),[{max_age, ?SESSION_AGE},{path, <<"/">>}])]
            end
    end.
