-module(tests).
-export([
    test/2,
    gen_uuid4/1,
    gen_uuid_rnd/1,
    guid_generation/1,
    get_instance/1,
    gi_crc/1,
    gi_cut/1,
    
    get_inst1/0,
    get_inst2/0,
    get_inst1_i/1,
    get_inst2_i/1,

    parse_cookie_cw/0,
    parse_cookie_cw_i/1,
    lt_parse_cookie/1,
    parse_cookie_lt/0,
    parse_cookie_lt_i/1,
    reverse/1,

    test_session_proc/0,
    test_session_proc/1,
    tsp1/1, tsp2/1,
    test_sign_verify/1,

    chash_test/0, 
    mcd_ring_test/1,
    mcd_ring_test/2,
    mcd_key_prop_test/2,
    chash_add_node/2,
    chash_ring_test/1,
    chash_ring_test/3,
    chash_key_prop_test/1,
    chash_key_prop_test/3,
    chash_ring_handler/2,
    chash_ring_handler_loop/1,
    mcache_driver_tests/1
]).


test(Message, Fun) ->
    Time1 = now(),
    Fun(),
    Time2 = now(),
    io:format("~p -> ~p ~n", [Message, timer:now_diff(Time2, Time1)]).
    

guid_generation("uuid4") -> test("uuid4", fun() -> tests:gen_uuid4(1000000) end);
guid_generation("rnd") -> test("uuid rnd", fun() -> tests:gen_uuid_rnd(1000000) end).

get_instance("crc") -> test("GI crc", fun() -> tests:gi_crc(1000000) end);
get_instance("cut") -> test("GI crc", fun() -> tests:gi_cut(1000000) end).

gi_crc(0) -> ok;
gi_crc(N) when is_integer(N),N>0 -> mijk_session:get_instance(mijk_session:generate_session_uuid(1)), gi_crc(N-1).

gi_cut(0) -> ok;
gi_cut(N) when is_integer(N),N>0 ->
    B = mijk_session:generate_session_uuid(1),
    C = list_to_binary(integer_to_list(random:uniform(1000))),
    mijk_session:get_instance_cut(<<B/binary, "-", C/binary>>), gi_cut(N-1).
    
gen_uuid4(0) -> ok;
gen_uuid4(N) when is_integer(N),N>0 -> mijk_session:generate_session_uuid(1), gen_uuid4(N-1).

gen_uuid_rnd(0) -> ok;
gen_uuid_rnd(N) when is_integer(N),N>0 -> mijk_session:generate_session_uuid(), gen_uuid_rnd(N-1).

get_inst1() ->
    mijk_session_manager:update_mc_pools([test1, test2, test3, test4, test5]),
    test("get_inst1 -> ", fun()-> tests:get_inst1_i(1000000) end).
get_inst2() ->
    mijk_session_manager:update_mc_pools([test1, test2, test3, test4, test5]),
    test("get_inst2 -> ", fun()-> tests:get_inst2_i(1000000) end).

get_inst1_i(0) -> ok;
get_inst1_i(N) -> mijk_session_manager:get_instance(mijk_session:generate_session_uuid(1)), get_inst1_i(N-1).

get_inst2_i(0) -> ok;
get_inst2_i(N) -> mijk_session_manager:get_instance(mijk_session:generate_session_uuid(1), 5), get_inst2_i(N-1).

parse_cookie_cw() -> test("cookie parse cw", fun() -> tests:parse_cookie_cw_i(1000000) end).

parse_cookie_cw_i(0) -> ok;
parse_cookie_cw_i(N) ->
	C1 = <<"$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\";
	Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\";
	Shipping=\"FedEx\"; $Path=\"/acme\"">>,
    cowboy_cookies:parse_cookie(C1)
    , parse_cookie_cw_i(N - 1).
    

parse_cookie_lt() -> test("cookie parse lt", fun() -> tests:parse_cookie_lt_i(1000000) end).

parse_cookie_lt_i(0) -> ok;
parse_cookie_lt_i(N) ->
	C1 = <<"$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\";
	Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\";
	Shipping=\"FedEx\"; $Path=\"/acme\"">>,
    %io:format("PCLI-> ~p ~n", [N]),
    lt_parse_cookie(C1)
    , parse_cookie_lt_i(N - 1).

lt_parse_cookie(<<"">>)    -> [];
lt_parse_cookie(undefined) -> [];
lt_parse_cookie([])        -> [];
lt_parse_cookie(String) when is_binary(String)->
    lists:flatten([lt_parse_cookie_i(X) || X <- binary:split(String, <<";">>, [global]), X=/=<<>>]).

lt_parse_cookie_i(<<$$,_/binary>>) -> [];
lt_parse_cookie_i(S) ->
    case binary:split(S, <<"=">>) of
        [K, V]    -> {lt_c_cl(K), lt_c_cl(V)};
        [K, <<>>] -> {lt_c_cl(K), []};
        [K]       -> {lt_c_cl(K), []}
    end.

lt_c_cl(String) ->
    S1 = skip_whitespace(String),
    reverse(skip_whitespace(reverse(S1))).

skip_whitespace(String) -> binary_dropwhile(fun is_whitespace/1, String).

binary_dropwhile(_, <<"">>) -> <<"">>;
binary_dropwhile(F, <<C, Rest/binary>> = String) ->
    case F(C) of
	    true  -> binary_dropwhile(F, Rest);
	    false -> String
    end.

is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\r) -> true;
is_whitespace($\n) -> true;
is_whitespace($")  -> true;
is_whitespace(_)   -> false.

reverse(Binary) -> 
    list_to_binary(lists:reverse(binary_to_list(Binary))).
%reverse(Binary) ->
%    Size = size(Binary)*8,
%    <<T:Size/integer-little>> = Binary,
%    <<T:Size/integer-big>>.

%lt_c_cl(String) when size(String) > 3 ->
%    {match, [[Trimmed]]} = re2:match(String, "^\\s*\"*([^\\s\"]+.+[^\\s\"]+)\"*\\s*$", 
%        [{capture, all_but_first, binary}, global, dollar_endonly, unicode, dotall]), Trimmed;
%lt_c_cl(String) when size(String) > 3 ->
%    {match, [Trimmed]} = re2:match(String, "^\\s*\"*([^\\s\"]+.+[^\\s\"]+)\"*\\s*$", 
%        [{capture, all_but_first, binary}]), Trimmed;
%lt_c_cl(<<$",R/utf8,$">>) -> <<R>>;
%lt_c_cl(String)           -> re2:replace(String, ["\\s"], [], [global, {return, binary}]).
%lt_c_cl(String)           -> re2:replace(String, ["\\s"], [], []).

-define(Req, {req,'POST',
         [<<"poll">>],
         [],<<"/poll">>,
         {1,1},
         [{<<"Cookie2">>,<<"$Version=\"1\"">>},
          {<<"Cookie">>,<<"MIJKSSID=22a2748b-6216-4748-aa49-12a0ceed0b0d">>},
          {<<"Content-Type">>,<<"application/x-www-form-urlencoded">>},
          {<<"Content-Length">>,<<"101">>},
          {<<"User-Agent">>,<<"libwww-perl/6.03">>},
          {<<"Host">>,<<"localhost:3030">>},
          {<<"Connection">>,<<"TE, close">>},
          {<<"Te">>,<<"deflate,gzip;q=0.3">>}],
         <<"request=%7B%22type%22%3A%22poll%22%2C%22seq%22%3A0%2C%22channels%22%3A%5B%22ch1%22%2C+%22ch2%22%5D%7D">>,
         "<0.333.0>",""}).

test_session_proc() ->
    test("MCACHE:", fun() -> tsp1(1000) end),
    test("SIGN:  ",   fun() -> tsp2(1000) end).

test_session_proc(N) ->
    test("MCACHE:", fun() -> tsp1(N) end),
    test("SIGN:  ",   fun() -> tsp2(N) end).

tsp1(0) -> ok;
tsp1(N) ->
    mijkweb_session:session_process(elli, ?Req, []),    
    tsp1(N-1).

tsp2(0) -> ok;
tsp2(N) ->
    mijkweb_session:session_process(ellic, ?Req, []),    
    tsp2(N-1).

test_sign_verify(N) ->
    test("SIGN  :", fun() -> t_sign(N) end),
    test("VERIFY:", fun() -> t_sign_ver(N) end),
    test("JSONTO:", fun() -> t_jsnto(N) end),
    test("JSFROM:", fun() -> t_jsnfrom(N) end),
    test("COOKTO:", fun() -> t_cookto(N) end),
    test("COFROM:", fun() -> t_cookfrm(N) end).

t_sign(0) -> ok;
t_sign(N) ->
    auth_utils:sign(<<"{\"ts\":1234567890, \"counter\":10}">>),
    t_sign(N-1).

t_sign_ver(0) -> ok;
t_sign_ver(N) ->
    auth_utils:check_sign(<<"{\"ts\":1234567890, \"counter\":10}">>, <<"MCwCFDeHSoMf0fyfrQsDOrYKtU4T9TjjAhRUDfN35o%2FIiHcIQO02KC3uXZrzZA%3D%3D">>),
    t_sign_ver(N-1).

t_jsnto(0) -> ok;
t_jsnto(N) ->
    jiffy:encode({[{<<"ts">>, 1234567890},{<<"counter">>, 10}]}),
    t_jsnto(N-1).

t_jsnfrom(0) -> ok;
t_jsnfrom(N) ->
    jiffy:decode(<<"{\"ts\":1234567890, \"counter\":10}">>),
    t_jsnfrom(N-1).

t_cookto(0) -> ok;
t_cookto(N) ->
    mijkweb_utils:cookie_encode(<<"{\"ts\":1234567890, \"counter\":10}">>),
    t_cookto(N-1).

t_cookfrm(0) -> ok;
t_cookfrm(N) ->
    mijkweb_utils:cookie_decode(<<"eyJ0cyI6MTIzNDU2Nzg5MCwgImNvdW50ZXIiOjEwfQ%3D%3D">>),
    t_cookfrm(N-1).


chash_test() ->
    Fun = fun(Node, Chash) ->
        chash:merge_rings(Chash, chash:fresh(256, Node))
    end,
    CH = lists:foldl(Fun, chash:fresh(256, 1), lists:seq(2,10)),
    io:format("Nodes ~p ~n", [chash:nodes(CH)]),
    io:format("Members ~p ~n", [chash:nodes(CH)]),
    io:format("CH ~p ~n", [CH]).

chash_add_node({N, RawRing} = R, NewNode) ->
    Members = chash:members(R),
    random:seed(),
    NewNodeCount = N div (length(Members) + 1),
    FunM = fun({Point, Node}, {AC, NR}) ->
        case {proplists:get_value(Node, AC, 0), proplists:get_value(NewNode, AC, 0)} of
            {C, B} when C >= NewNodeCount, B < NewNodeCount
               -> {[{NewNode, B + 1}] ++ AC, [{Point, NewNode}] ++ NR}
            ;{C, _} when C < NewNodeCount 
               -> {[{Node, C + 1}] ++ AC, [{Point, Node}] ++ NR}
            ;_ -> 
                  I = random:uniform(length(Members) + 1),
                  RNode = lists:nth(I, [NewNode] ++ Members),
                  {[{RNode, proplists:get_value(RNode, AC, 0) + 1}] ++ AC, [{Point, RNode}] ++ NR}
        end
    end,
    {_, NRR} = lists:foldl(FunM, {[], []}, RawRing),
    {N, lists:reverse(NRR)}.

chash_ring_test(N) -> chash_ring_test(N, 64, 6).
chash_ring_test(N, RingSize, Points) ->
    Fun = fun(K, R) -> tests:chash_add_node(R, K)  end,
    Ring = lists:foldl(Fun, chash:fresh(RingSize, 1), lists:seq(2, Points)),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    chash_ring:start_link([RingSize, Points]),
    Pid = chash_ring_handler(RingSize, Points),
    ets:new(ring_ets, [named_table, set, private]),
    ets:insert(ring_ets, {1, Ring}),
    test("chash lookup: ", fun() -> t_chash_r_test(Ring, N) end),
    test("chash lookup1:", fun() -> t_chash_r_test1(N) end),
    test("chash lookup2:", fun() -> t_chash_r_test2(N, Pid, self()) end),
    test("chash lookup3:", fun() -> t_chash_r_test3(N) end).

t_chash_r_test(_, 0)    -> ok;
t_chash_r_test(Ring, N) ->
    chash:successors(chash:key_of(random:uniform(10000000000)), Ring),
    t_chash_r_test(Ring, N-1).

t_chash_r_test1(0) -> ok;
t_chash_r_test1(N) ->
    chash_ring:lookup(random:uniform(10000000000)),
    t_chash_r_test1(N-1).

t_chash_r_test2(0, _, _) -> ok;
t_chash_r_test2(N, P, O) ->
    P ! {lookup, random:uniform(10000000000), O},
    receive
        _ -> ok
    after 1000 -> io:format("TO! ~n", []), ok
    end,
    t_chash_r_test2(N-1, P, O).

t_chash_r_test3(0) -> ok;
t_chash_r_test3(N) ->
    [{_, Ring}] = ets:lookup(ring_ets, 1),
    chash:successors(chash:key_of(random:uniform(10000000000)), Ring),
    t_chash_r_test3(N-1).

chash_key_prop_test(N) -> chash_key_prop_test(N, 64, 6).
chash_key_prop_test(N, RingSize, Points) ->
    Fun = fun(K, R) -> tests:chash_add_node(R, K)  end,
    Ring = lists:foldl(Fun, chash:fresh(RingSize, 1), lists:seq(2, Points)),
    chash_key_prop_test(proc, N, [], Ring).
chash_key_prop_test(_, 0, Acc, _)       -> Acc;
chash_key_prop_test(proc, N, Acc, Ring) ->
    Key = mijk_session:generate_session_uuid(1),
    [{_,H}|_] = chash:successors(chash:key_of(Key), Ring),
    case proplists:get_value(H, Acc, undefined) of
        undefined -> chash_key_prop_test(proc, N - 1, [{H, 1}] ++ Acc, Ring);
        Count     -> chash_key_prop_test(proc, N - 1, [{H, Count+1}] ++ proplists:delete(H, Acc), Ring)
    end.

mcd_ring_test(N) -> mcd_ring_test(N, [{n1, n1, 10}, {n2, n2, 10}, {n3, n3, 10}, {n4, n4, 10}, {n5, n5, 10}, {n6, n6, 10}]).
mcd_ring_test(N, Ring) ->
    {ok, Pid} = dht_ring:start_link(Ring),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    test("mcd lookup:", fun() -> t_mcd_r_test(Pid, N) end),
    test("random test:", fun() -> t_mcd_rand(N) end).

t_mcd_rand(0) -> ok;
t_mcd_rand(N) ->
    random:uniform(10000000000),
    t_mcd_rand(N-1).

t_mcd_r_test(_,   0) -> ok;
t_mcd_r_test(Pid, N) ->
    dht_ring:lookup(Pid, random:uniform(10000000000)),
    t_mcd_r_test(Pid, N-1).

%tests:mcd_key_prop_test(10000, [{n1,n1,50}, {n2,n2,50}, {n3,n3,50}, {n4,n4,50}, {n5,n5,50}, {n6,n6,50}, {n7,n7,50},{n8,n8,50},{n9,n9,50},{n10,n10,50}]).
mcd_key_prop_test(N, Ring)         ->
    {ok, Pid} = dht_ring:start_link(Ring),
    mcd_key_prop_test(N, [], Pid). 
mcd_key_prop_test(0, A, _)   -> A;
mcd_key_prop_test(N, A, Mcd) ->
    Key = mijk_session:generate_session_uuid(1),
    [H|_] = dht_ring:lookup(Mcd, Key),
    case proplists:get_value(H, A, undefined) of
        undefined -> mcd_key_prop_test(N - 1, [{H, 1}] ++ A, Mcd);
        Count     -> mcd_key_prop_test(N - 1, [{H, Count+1}] ++ proplists:delete(H, A), Mcd)
    end.

chash_ring_handler(RingSize, Points) ->
    Fun = fun(K, R) -> tests:chash_add_node(R, K)  end,
    Ring = lists:foldl(Fun, chash:fresh(RingSize, 1), lists:seq(2, Points)),
    spawn_link(tests, chash_ring_handler_loop, [Ring]).

chash_ring_handler_loop(Ring) ->
    receive
        {lookup, Key, Pid} ->
            Pid ! chash:successors(chash:key_of(Key), Ring),
            chash_ring_handler_loop(Ring);
        {get_ring, Pid}    ->
            Pid ! Ring,
            chash_ring_handler_loop(Ring);
        _Other             ->
            io:format("Unexp message: ~p ~n", [_Other])
    after 5000 -> chash_ring_handler_loop(Ring)
    end.

mcache_driver_tests(N) ->
    test("mcache test :", fun() -> mdtest1(N) end),
    test("mcd: test   :", fun() -> mdtest2(N) end),
    test("mcache test :", fun() -> mdtest1(N) end),
    test("mcd: test   :", fun() -> mdtest2(N) end).

mdtest1(0) -> ok;
mdtest1(N) ->
    SSID = mijk_session:generate_session_uuid(1),
    mcache:set_raw(SSID, 
        mijk_session:erl_to_mysql([{<<"lalala">>, 1}, {<<"gugugu">>}, 2]), raw, 1200),
    mcache:get_raw(<<"">>, SSID),
    mdtest1(N-1).

mdtest2(0) -> ok;
mdtest2(N) ->
    SSID = mijk_session:generate_session_uuid(1),
    mcd:ldo(set, SSID, mijk_session:erl_to_mysql([{<<"lalala">>, 1}, {<<"gugugu">>}, 2]), 0, 1200),
    mcd:get(SSID),
    mdtest2(N-1).
    
