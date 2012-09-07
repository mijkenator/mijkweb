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
    get_inst2_i/1
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

