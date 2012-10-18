-module(riak_man_test).

-export([
    test1/0,
    insert_test_data/2,
    insert_test_data_2i/2,
    stream_mr1/0, stream_mr2/0,
    stream_mr/3, stream_mr/4, mr/3,
    insert_linked_data/3, insert_linked_data/0
    ]).

test1() ->
    riak_man:set_bucket_backend("127.0.0.1", "8098", "buck_bk", "bitcask_mult"),
    riak_man:set_bucket_backend("127.0.0.1", "8098", "buck_ld", "eleveldb_mult"),
    riak_man:set_bucket_backend("127.0.0.1", "8098", "buck_me", "memory_mult"),

    insert_test_data(<<"buck_bk">>, 100000),
    insert_test_data_2i(<<"buck_ld">>, 100000),
    insert_test_data(<<"buck_me">>, 100000),
    test_searches(<<"session_buk">>, 1000),
    delete_test_data(<<"sessions_buk">>).


insert_linked_data() ->
    riak_man:set_bucket_backend("127.0.0.1", "8098", "link_ld1", "eleveldb_mult"),
    riak_man:set_bucket_backend("127.0.0.1", "8098", "link_ld2", "eleveldb_mult"),
    insert_linked_data(<<"link_ld1">>, <<"link_ld2">>, 100).


%
%Inputs = [{<<"link_ld1">>, <<"10">>}].
%f(MR), MR = [{link, '_', '_', true}, {map, {modfun, riak_man_mr_funcs, obj_value}, none, true}].
%f(MR), MR = [{link, '_', '_', true}, {map, {modfun, riak_man_mr_funcs, linked_objects}, none, true}]. -- nested MR for getting linked objects
%riak_man:method(pool1, mapred, [Inputs, MR]).
%
insert_linked_data(_, _, 0) -> ok;
insert_linked_data(MainBucket, LinkedBucket, I) ->
    SSID = mijk_session:generate_session_uuid(1),
    {Ms, S, MicS} = now(),
    Time = Ms * 1000000 + S,
    RndIntConst = random:uniform(100),

    MetaData = dict:store(<<"index">>, [{<<"rnd_int">>, RndIntConst}, {<<"time_int">>, Time}], dict:new()),
    MetaData1 = dict:store(<<"Links">>, [{{LinkedBucket, list_to_binary(integer_to_list(I))}, <<"contact_list">>}], MetaData),
    Object   = riakc_obj:new(MainBucket, list_to_binary(integer_to_list(I)), {session, SSID, {Time, MicS}, RndIntConst, []}),
    Object1  = riakc_obj:update_metadata(Object, MetaData1),

    RndIntList = [ random:uniform(100) || _ <- lists:seq(1,5)],
    Object2   = riakc_obj:new(LinkedBucket, list_to_binary(integer_to_list(I)), {link, <<"link_ld3">>, RndIntList}),
    
    riak_man:put(pool1, Object1, []),
    riak_man:put(pool1, Object2, []),

    insert_linked_data(MainBucket, LinkedBucket, I - 1).
    
   
insert_test_data(_, 0) -> ok;
insert_test_data(Bucket, I) ->
    SSID = mijk_session:generate_session_uuid(1),
    {Ms, S, MicS} = now(),
    Time = Ms * 1000000 + S,
    RndIntConst = random:uniform(100),
    riak_man:store(pool1, Bucket, list_to_binary(integer_to_list(I)), {session, SSID, {Time, MicS}, RndIntConst, []},[]),
    insert_test_data(Bucket, I - 1).

test_searches(_,_) -> ok.

delete_test_data(_) -> ok.

insert_test_data_2i(_, 0) -> ok;
insert_test_data_2i(Bucket, I) ->
    SSID = mijk_session:generate_session_uuid(1),
    {Ms, S, MicS} = now(),
    Time = Ms * 1000000 + S,
    RndIntConst = random:uniform(100),

    MetaData = dict:store(<<"index">>, [{<<"rnd_int">>, RndIntConst}, {<<"time_int">>, Time}], dict:new()),
    Object   = riakc_obj:new(Bucket, list_to_binary(integer_to_list(I)), {session, SSID, {Time, MicS}, RndIntConst, []}),
    Object1  = riakc_obj:update_metadata(Object, MetaData),

    riak_man:put(pool1, Object1, []),
    insert_test_data_2i(Bucket, I - 1).

clean_all() ->
    Fun = fun(Bucket) ->
        Keys = riak_man:list_keys(pool1, Bucket),
        lists:foreach(fun(Key)-> riak_man:method(pool1, delete, [Bucket, Key]) end, Keys)
    end,
    {ok, Buckets} = riak_man:list_buckets(pool1, []),
    lists:foreach(Fun, riak_man:list_buckets(pool1, Buckets)).

   
stream_mr1() ->
    Inputs = [{<<"buck_ld">>, list_to_binary(integer_to_list(X))} || X <-lists:seq(1,10000)],
    Query  = [{map, {modfun, riak_kv_mapreduce, map_object_value}, <<"filter_notfound">>, true}],
    riak_man:method(pool1, mapred_stream, [Inputs, Query, self()]),
    stream_loop().

stream_mr2() ->
    Query  = [{map, {modfun, riak_kv_mapreduce, map_object_value}, <<"filter_notfound">>, true}],
    riak_man:method(pool1, mapred_bucket_stream, [<<"buck_ld">>, Query, self(), 60000]),
    stream_loop().
   
stream_mr(Bucket, Count, Func) ->   
    Inputs = [{Bucket, list_to_binary(integer_to_list(X))} || X <-lists:seq(1,Count)],
    %Query  = [{map, {modfun, riak_kv_mapreduce, map_object_value}, <<"filter_notfound">>, true}],
    Query = [{map, {modfun, riak_man_mr_funcs, Func}, none, true}],
    riak_man:method(pool1, mapred_stream, [Inputs, Query, self()]),
    stream_loop().

stream_mr(Bucket, Count, Map, Reduce) ->   
    Inputs = [{Bucket, list_to_binary(integer_to_list(X))} || X <-lists:seq(1,Count)],
    Query = Map ++ Reduce,
    riak_man:method(pool1, mapred_stream, [Inputs, Query, self()]),
    stream_loop().

mr(Bucket, Count, MR) ->
    Inputs = [{Bucket, list_to_binary(integer_to_list(X))} || X <-lists:seq(1,Count)],
    riak_man:method(pool1, mapred, [Inputs, MR]).
    

%% @private
stream_loop() ->
    receive
        {_ReqId, done} ->
            io:format("job done ~n"), ok;
        {_ReqId, {mapred,_Phase,Results}} ->
            io:format("Streaming Results: ~p~n", [Results]),
            stream_loop();
        {_ReqId, {error, Reason}} ->
            io:format("Something bad happened! ~p~n", [Reason])
    end.

% riak_man:method(pool1, get_index, [<<"sss2">>, <<"rnd_int">>, 1]).
% riak_man:method(pool1, get_bucket, [<<"sss2">>]])
%
%
% riak_core_bucket:set_bucket(<<"test_bitcask">>, [{backend, <<"bitcask_mult">>}]).
% riak_core_bucket:set_bucket(<<"test_levdb">>, [{backend, <<"eleveldb_mult">>}]).
% riak_core_bucket:set_bucket(<<"test_memdb">>, [{backend, <<"memory_mult">>}]).
