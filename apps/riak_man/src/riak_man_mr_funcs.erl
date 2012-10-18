-module(riak_man_mr_funcs).

-export([
    count/3,
    test1/3,
    test2/3,
    test3/3, test31/3,
    test4/3,
    test5/3,
    obj_value/3,
    linked_objects/3
]).


obj_value(G, _,_) ->
    [binary_to_term(riak_object:get_value(G))].

%Inputs = [{<<"link_ld1">>, <<"10">>}].
%f(MR), MR = [{link, '_', '_', true}, {map, {modfun, riak_man_mr_funcs, linked_objects}, none, true}].
%riak_man:method(pool1, mapred, [Inputs, MR]).

linked_objects(G, _, _) ->
    BObj =  binary_to_term(riak_object:get_value(G)),
    IdList = element(3, BObj),
    Inputs = [{<<"link_ld2">>, list_to_binary(integer_to_list(X))} || X <- IdList],
    MR = [{map, {modfun, riak_man_mr_funcs, obj_value}, none, true}],
    {ok, C} = riak:local_client(),
    {ok, Ret} = C:mapred(Inputs, MR),
    Ret.
    

count(G, undefined, none) ->
    [element(2, binary_to_term(riak_object:get_value(G)))].

test1(Obj, KeyData, Arg) ->
    [{element(2, binary_to_term(riak_object:get_value(Obj))), KeyData, Arg}].
test2(Obj, _KeyData, _Arg) ->
    BObj =  binary_to_term(riak_object:get_value(Obj)),
    if element(4, BObj) =:= 50 -> [element(2, BObj)]; true -> [] end.
test3(Obj, _KeyData, _Arg) ->
    BObj =  binary_to_term(riak_object:get_value(Obj)),
    {_, Time} = element(3, BObj),
    if
        (Time rem 5) =:= 0 -> [{{riak_object:bucket(Obj), riak_object:key(Obj)}, BObj}]
        ;true              -> []
    end.
test31(Obj, _KeyData, _Arg) ->
    BObj =  binary_to_term(riak_object:get_value(Obj)),
    {Time,_} = element(3, BObj),
    [{{riak_object:bucket(Obj), riak_object:key(Obj)}, Time}].
test4(_Obj, KeyData, _) ->
    if element(4, KeyData) =:= 50 -> [element(2, KeyData)]; true -> [] end.
test5(Obj, _KeyData, _Arg) ->
    BObj =  binary_to_term(riak_object:get_value(Obj)),
    [{{riak_object:bucket(Obj), riak_object:key(Obj)}, BObj}].
