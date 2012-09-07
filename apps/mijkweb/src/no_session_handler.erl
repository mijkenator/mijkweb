-module(no_session_handler).
-export([init/3, handle/2, terminate/2]).

-include("../../deps/cowboy/include/http.hrl").
-include("include/consts.hrl").

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.
    
handle(#http_req{method=Method, raw_path=RPath} = Req, State) ->
    lager:debug("REQ: ~p -> ~p ~n", [Method, RPath]),
    {ok, Req2} = cowboy_http_req:reply(200, [], <<"Kolobok alive!">>, Req),
    {ok, Req2, State}.

terminate(_Req, _State) -> ok.