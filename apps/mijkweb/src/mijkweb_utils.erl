-module(mijkweb_utils).

-export([
    now/0,
    merge_plists/2,
    plist_values/2,
    cookie_encode/1,
    cookie_decode/1
]).

-compile({no_auto_import,[now/0]}).

-spec now() -> integer().
now() ->
    {Ms, S, _} = erlang:now(),
    1000000 * Ms + S.

-spec merge_plists(list(), list()) -> list().
merge_plists([], []) -> [];
merge_plists(L1, []) -> L1;
merge_plists([], L2) -> L2;
merge_plists(L1, L2) ->
    Fun = fun({Key, Value}, A)  ->
        case proplists:is_defined(Key, A) of
            false -> [{Key, Value}] ++ A;
            true  -> [{Key, Value}] ++ proplists:delete(Key, A)
        end
    end,
    lists:foldl(Fun, L1, L2).

-spec plist_values(list(), list()) -> list().
plist_values(PropList, KeyDefList) ->
    lists:map(fun({K,V})->  proplists:get_value(K, PropList, V) end, KeyDefList).

-spec cookie_encode(binary()) -> binary().
cookie_encode(C) when is_binary(C) ->
    list_to_binary(http_uri:encode(base64:encode_to_string(C))).

-spec cookie_decode(binary()) -> binary().
cookie_decode(C) when is_binary(C) ->
    base64:decode(http_uri:decode(binary_to_list(C))).
