-module(elli_pirogok).
-export([handle/2, handle_event/3]).

-include("include/consts.hrl").
-include("include/mijkweb_consts.hrl").
-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle(ReqMethod, [<<"auth">>|_] = Path, Req) ->
    case mijkweb_session:check_auth(elli, Req) of
        {ok, SessionData} -> auth_handle(ReqMethod, Path, Req, SessionData)
        ;_                -> {ok, [], mijkweb_response:json_error_response(?AUTH_REQUIRED)}
    end;

handle('POST', [<<"auth">>], Req) ->
    try
        Request = cowboy_http:urldecode(elli_request:post_arg(<<"request">>, Req, <<"{}">>)),
        Json    = jiffy:decode(Request),
        [Type, Login, Password] = [proplists:get_value(X, Json, undefined) || X <- [<<"type">>, <<"login">>, <<"password">>]],
        is_binary(Login) orelse throw({error, nologin}), 
        is_binary(Password) orelse throw({error, nopassword}),
        Type =:= <<"login">> orelse throw({error, badcommand}),
        case auth_utils:check_credentials(Login, Password) of
            {ok, _} ->
                       [_, RespHeaders] = mijkweb_session:session_process(elli, Req, 
                       [{<<"status">>, <<"authorized">>}, 
                        {<<"type">>, <<"user">>},
                        {<<"user_id">>, <<"mimimi">>}]),
                       Response = {[
                            {<<"type">>, Type},
                            {<<"status">>, <<"ok">>}
                       ]},
                       {ok, RespHeaders, mijkweb_response:json_ok_response(Response)}
            ;_      -> {ok, [], mijkweb_response:json_error_response(?AUTH_FAILED)}
        end
    catch
        throw:{error, nologin}    -> {ok, [], mijkweb_response:json_error_response(?AUTH_NOLOGIN)};
        throw:{error, nopassword} -> {ok, [], mijkweb_response:json_error_response(?AUTH_NOPASSWORD)};
        throw:{error, badcommand} -> {ok, [], mijkweb_response:json_error_response(?BADCOMMAND)};
        Error:Reason              -> 
                                     lager:error("~p ~p ", [Error, Reason]),
                                     {ok, [], mijkweb_response:json_error_response(?UNKNOWN_ERROR)} 
    end;

handle(_, _, _Req) ->
    {404, [], <<"Ты кто такой, давай досвидания!">>}.

auth_handle(ReqMethod, Path, Req, SessionData) ->
    lager:error("unknown auth method: ~p ~p ~p ~p", [ReqMethod, Path, Req, SessionData]),
    {404, [], <<"Ты кто такой, давай досвидания!">>}.


%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) -> ok.

