-module(mijkweb_app).   

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = [
    %% {Host, list({Path, Handler, Opts})}
       {'_', [
        {[<<"test1">>], no_session_handler, []},
        {[<<"test2">>], quick_session_handler, []},
        {[<<"test3">>], full_session_handler, []},
        {[<<"test4">>], mcd_session_handler, []},
        {[<<"test">>,<<"cookie">>], set_cookie_handler, []},
        {'_', def_handler, []}
       ]}
    ],
    %cowboy:start_listener(my_http_listener, 100,
    %   cowboy_tcp_transport, [{port, 8080}],
    %   cowboy_http_protocol, [{dispatch, Dispatch}]
    %),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}], [{dispatch, Dispatch}]),	
    elli:start_link([{callback, elli_pirogok_callback}, {port, 3030}]),
    elli:start_link([{callback, elli_pirogok}, {port, 3033}]),
    mijk_session:init(),

    case application:get_env(mijkweb, mysql_db_config) of
        {ok, [_|_] = L} ->
            emysql:add_pool(mysqlpool,
                proplists:get_value("processes", L, 20),
                proplists:get_value("user", L, "mijkweb"),
                proplists:get_value("password", L, "mijkweb"),
                proplists:get_value("host", L, "localhost"),
                proplists:get_value("port", L, 3306),
                proplists:get_value("database", L, "mijkweb"), utf8)
        ;_              -> exit("No mysql connection config")
    end,
    catch(ex_reloader:start()),
    mijkweb_sup:start_link().

stop(_State) ->
    ok.
