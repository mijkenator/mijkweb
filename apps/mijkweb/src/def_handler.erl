-module(def_handler).
-export([init/3, handle/2, terminate/2]).
-export([
    dirty_get_session_header/1,
    get_session_header/1
]).

-include("include/consts.hrl").

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(#http_req{method=Method, path=RPath} = Req, State) ->
    %Headers = dirty_get_session_header(Req),
    %Headers = mysql_get_session_header(Req),
    Headers = mcache_get_session_header(Req),
    lager:debug("REQ: ~p -> ~p ~n", [Method, RPath]),
    IpS = list_to_binary(io_lib:format("~p", [inet:getifaddrs()])), 
    {ok, Req2} = cowboy_req:reply(200, Headers, <<"Kolobok alive!\n<br>", IpS/binary>>, Req),
    {ok, Req2, State}.
    
%handle(#http_req{method=Method, raw_path=RPath} = Req, State) ->
%    lager:debug("REQ: ~p -> ~p ~n", [Method, RPath]),
%    {ok, Req2} = cowboy_http_req:reply(200, [], <<"Kolobok alive!">>, Req),
%    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

%
%Memcached version. try to det session cookie, if not create one. Update sessions expiration date
%
-spec mcache_get_session_header(#http_req{}) -> kvlist().
mcache_get_session_header(Req) ->
    case cowboy_req:cookie(<<"MIJKSSID">>, Req, undefined) of
        {undefined, _} ->
            [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:mcache_create_session(1),[{max_age, ?SESSION_AGE}])];
        {SSID, _}      ->
            case mijk_session:mcache_check_session_data(SSID) of
                {ok, Data} when is_binary(Data) ->
                    EData = mijk_session:mysql_to_erl(Data),
                    NewData = case proplists:get_value(<<"counter">>, EData, undefined) of
                        undefined            -> [{<<"counter">>, 1}] ++ EData;
                        V when is_integer(V) -> lists:keyreplace(<<"counter">>, 1, EData, {<<"counter">>, V+1})
                    end,
                    mijk_session:mcache_update_session(SSID, NewData),
                    [cowboy_cookies:cookie(<<"MIJKSSID">>, SSID,[{max_age, ?SESSION_AGE}])]
                ;_ ->
                    [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:mcache_create_session(1),[{max_age, ?SESSION_AGE}])]
            end
    end.

%
%Mysql version. try to det session cookie, if not create one. Update sessions expiration date
%
-spec mysql_get_session_header(#http_req{}) -> kvlist().
mysql_get_session_header(Req) ->
    case cowboy_req:cookie(<<"MIJKSSID">>, Req, undefined) of
        {undefined, _} ->
            [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:mysql_create_session(1),[{max_age, ?SESSION_AGE}])];
        {SSID, _}      ->
            case mijk_session:mysql_check_session_data(SSID) of
                {ok, undefined} -> 
                    mijk_session:mysql_update_session(SSID, [{<<"counter">>, 1}]),
                    [cowboy_cookies:cookie(<<"MIJKSSID">>, SSID,[{max_age, ?SESSION_AGE}])];
                {ok, <<>>}      -> 
                   mijk_session:mysql_update_session(SSID, [{<<"counter">>, 1}]),
                   [cowboy_cookies:cookie(<<"MIJKSSID">>, SSID,[{max_age, ?SESSION_AGE}])]; 
                {ok, Data} when is_binary(Data) ->
                    EData = mijk_session:mysql_to_erl(Data),
                    NewData = case proplists:get_value(<<"counter">>, EData, undefined) of
                        undefined            -> [{<<"counter">>, 1}] ++ EData;
                        V when is_integer(V) -> lists:keyreplace(<<"counter">>, 1, EData, {<<"counter">>, V+1})
                    end,
                    mijk_session:mysql_update_session(SSID, NewData),
                    [cowboy_cookies:cookie(<<"MIJKSSID">>, SSID,[{max_age, ?SESSION_AGE}])]
                ;_ -> [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:mysql_create_session(1),[{max_age, ?SESSION_AGE}])]
            end
    end.

%
%Dirty version. try to det session cookie, if not create one. Update sessions expiration date
%
-spec dirty_get_session_header(#http_req{}) -> kvlist().
dirty_get_session_header(Req) ->
    case cowboy_req:cookie(<<"MIJKSSID">>, Req, undefined) of
        {undefined, _} ->
            [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:create_session(1),[{max_age, ?SESSION_AGE}])];
        {SSID, _}      ->
            case mijk_session:dirty_check_session_data(SSID) of
                {ok, Data} ->
                    NewData = case proplists:get_value(<<"counter">>, Data, undefined) of
                        undefined            -> [{<<"counter">>, 1}] ++ Data;
                        V when is_integer(V) -> lists:keyreplace(<<"counter">>, 1, Data, {<<"counter">>, V+1})
                    end,
                    mijk_session:dirty_update_session(SSID, NewData),
                    [cowboy_cookies:cookie(<<"MIJKSSID">>, SSID,[{max_age, 1200}])]
                ;_ -> [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:create_session(1),[{max_age, ?SESSION_AGE}])]
            end
    end.
    
%
%try to det session cookie, if not create one. Update sessions expiration date
%
-spec get_session_header(#http_req{}) -> kvlist().
get_session_header(Req) ->
    case cowboy_req:cookie(<<"MIJKSSID">>, Req, undefined) of
        {undefined, _} ->
            [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:create_session(1),[{max_age, ?SESSION_AGE}])];
        {SSID, _}      ->
            case mijk_session:check_session_data(SSID) of
                {ok, Data} ->
                    NewData = case proplists:get_value(<<"counter">>, Data, undefined) of
                        undefined            -> [{<<"counter">>, 1}] ++ Data;
                        V when is_integer(V) -> lists:keyreplace(<<"counter">>, 1, Data, {<<"counter">>, V+1})
                    end,
                    mijk_session:update_session(SSID, NewData),
                    [cowboy_cookies:cookie(<<"MIJKSSID">>, SSID,[{max_age, 1200}])]
                ;_ -> [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:create_session(1),[{max_age, ?SESSION_AGE}])]
            end
    end.
