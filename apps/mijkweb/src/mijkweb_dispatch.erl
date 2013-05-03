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
        {'POST', [<<"auth">>, <<"check">>, <<"session">>], elli_user_controller, check_session},
        {'POST', [<<"auth">>, <<"poll">>], elli_user_controller, poll},
        {'POST', [<<"auth">>, <<"push">>], elli_user_controller, push},
        {'POST', [<<"auth">>, <<"user">>, <<"profile">>], elli_user_controller, user_profile},
        {'POST', [<<"auth">>, <<"user">>, <<"set">>], elli_user_controller, set_key},
        {'POST', [<<"auth">>, <<"user">>, <<"get">>], elli_user_controller, get_key},
        {'POST', [<<"auth">>, <<"user">>, <<"del">>], elli_user_controller, del_key},
        {'POST', [<<"auth">>, <<"admin">>, <<"user">>], elli_admin_user_controller, handler},
        {'POST', [<<"auth">>, <<"admin">>, <<"grid">>], elli_admin_grid, handler},
        {'POST', [<<"auth">>, <<"admin">>, <<"appuserdetails">>], elli_admin_appuserdetails, handler},
        {'POST', [<<"auth">>, <<"admin">>, <<"user">>, <<"delete">>], elli_user_controller, register_user},
        {'POST', [<<"auth">>, <<"admin">>, <<"user">>, <<"list">>], elli_user_controller, register_user},
        {'POST', [<<"auth">>, <<"admin">>, <<"user">>, <<"edit">>], elli_user_controller, register_user}
    ],
    mijkweb_utils:tlist_search(Routes, {Type, Path}).

