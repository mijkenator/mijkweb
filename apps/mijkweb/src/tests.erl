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
    test_sign_verify/1
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

lt_parse_cookie(String) ->
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

