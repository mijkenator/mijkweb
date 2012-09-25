-module(full_session_handler).

-export([init/3, handle/2, terminate/2]).
-export([
    mcache_get_session_header/1
]).

-include("include/consts.hrl").

%
%
% should use mcache sessions + redis|membase|mysql ???? (configurable)
%
%

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(#http_req{method=Method, path=RPath} = Req, State) ->
    Headers = ?MODULE:mcache_get_session_header(Req),
    lager:debug("REQ(fsh): ~p -> ~p ~n", [Method, RPath]),
    IpS = list_to_binary(io_lib:format("~p", [inet:getifaddrs()])), 
    {ok, Req2} = cowboy_req:reply(200, Headers, <<"Kolobok alive!\n<br>", IpS/binary>>, Req),
    {ok, Req2, State}.
    
terminate(_Req, _State) ->
    ok.


-spec mcache_get_session_header(#http_req{}) -> kvlist().
mcache_get_session_header(Req) ->
    lager:debug("COOKIE ~p ~n", [cowboy_req:cookie(<<"MIJKSSID">>, Req, undefined)]),
    case cowboy_req:cookie(<<"MIJKSSID">>, Req, undefined) of
        {undefined, _} ->
            [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:mcache_create_session(1),[{max_age, ?SESSION_AGE}])];
        {SSID, _}      ->
            case mijk_session:mcache_check_session_data_lock(SSID) of
                {ok, Data} when is_binary(Data) ->
                    EData = mijk_session:mysql_to_erl(Data),
                    NewData = case proplists:get_value(<<"counter">>, EData, undefined) of
                        undefined            -> [{<<"counter">>, 1}] ++ EData;
                        V when is_integer(V) -> lists:keyreplace(<<"counter">>, 1, EData, {<<"counter">>, V+1})
                    end,
                    mijk_session:mcache_update_session_lock(SSID, NewData),
                    [cowboy_cookies:cookie(<<"MIJKSSID">>, SSID,[{max_age, ?SESSION_AGE}])]
                ;{nok, lock_timeout}         -> throw("lock timeout")
                ;_ ->
                    %[cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:mcache_create_session(1),[{max_age, ?SESSION_AGE}])]
                    [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:mcache_create_session(SSID),[{max_age, ?SESSION_AGE}])]
            end
    end.
