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
       {'_', [{'_', def_handler, []}]}
    ],
    cowboy:start_listener(my_http_listener, 100,
       cowboy_tcp_transport, [{port, 8080}],
       cowboy_http_protocol, [{dispatch, Dispatch}]
    ),
    mijk_session:init(),
    timer:start(),
    mijk_session:clean_up_sessions_job(),
    mijkweb_sup:start_link().

stop(_State) ->
    ok.
