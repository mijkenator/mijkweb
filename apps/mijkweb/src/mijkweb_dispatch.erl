-module(mijkweb_dispatch).

-export([
    get_route/2
]).

-spec get_route('POST'|'GET', list()) -> {ok, atom(), atom()} | {error, binary()}.
get_route(Type, Path) ->
    Routes = [
        {'POST', [<<"user">>, <<"create">>], elli_user_controller, register_user},
        {'POST', [<<"app">>, <<"user">>, <<"create">>], elli_user_controller, register_app_user},
        {'POST', [<<"auth">>, <<"user">>, <<"create">>], elli_user_controller, register_app_user},
        {'POST', [<<"auth">>, <<"poll">>], elli_user_controller, poll},
        {'POST', [<<"auth">>, <<"pull">>], elli_user_controller, pull},
        {'POST', [<<"auth">>, <<"user">>, <<"profile">>], elli_user_controller, register_user},
        {'POST', [<<"auth">>, <<"user">>, <<"get">>], elli_user_controller, register_user},
        {'POST', [<<"auth">>, <<"user">>, <<"set">>], elli_user_controller, register_user},
        {'POST', [<<"auth">>, <<"admin">>, <<"user">>, <<"delete">>], elli_user_controller, register_user},
        {'POST', [<<"auth">>, <<"admin">>, <<"user">>, <<"list">>], elli_user_controller, register_user},
        {'POST', [<<"auth">>, <<"admin">>, <<"user">>, <<"edit">>], elli_user_controller, register_user}
    ],
    mijkweb_utils:tlist_search(Routes, {Type, Path}).

