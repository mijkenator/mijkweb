-module(set_cookie_handler).

-export([init/3, handle/2, terminate/2]).


-include("../../deps/cowboy/include/http.hrl").
-include("include/consts.hrl").

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(#http_req{method=Method, raw_path=RPath} = Req, State) ->
    random:seed(erlang:now()),
    RN = list_to_binary(integer_to_list(random:uniform(100))),
    Headers = [cowboy_cookies:cookie(<<"MIJKSSID">>, RN,[{max_age, ?SESSION_AGE}])],
    lager:debug("REQ(fsh): ~p -> ~p ~n", [Method, RPath]),
    IpS = list_to_binary(io_lib:format("~p", [inet:getifaddrs()])), 
    {ok, Req2} = cowboy_http_req:reply(200, Headers, <<"Kolobok alive!\n<br>", IpS/binary>>, Req),
    {ok, Req2, State}.
    
terminate(_Req, _State) ->
    ok.