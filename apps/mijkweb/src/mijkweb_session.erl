-module(mijkweb_session).

-export([
    session_process/2,
    session_process/3,
    session_process/4,
    check_auth/2,
    check_session/1,
    clean_session/2
]).

-include("include/consts.hrl").

%-spec session_process(atom(), any()) -> [binary(), list()].
session_process(elli, Req) -> session_process(ellimcd, Req, []).

%-spec session_process(atom(), any(), proplist()) -> [binary, list()].
session_process(ellimcd, Req, SessionData) ->
    Cookies = elli_request:get_header(<<"Cookie">>, Req, []),
    [SSID] = mijkweb_utils:plist_values(tests:lt_parse_cookie(Cookies), [{<<"MIJKSSID">>, <<"">>}]),
    session_process(ellimcd, Req, SessionData, SSID);

session_process({ellimcd, <<"auth">>}, Req, SessionData) ->
    Cookies = elli_request:get_header(<<"Cookie">>, Req, []),
    [SSID] = mijkweb_utils:plist_values(tests:lt_parse_cookie(Cookies), [{<<"MIJKSSID">>, <<"">>}]),
    session_process(ellimcd, Req, SessionData, {SSID, <<"auth">>});

session_process(elli, Req, SessionData) ->
    %lager:debug("MWS1i ~p", [SessionData]),
    Cookies = elli_request:get_header(<<"Cookie">>, Req, []),
    %lager:debug("ElliPOLL req2-1: ~p ~n", [Cookies]),
    [{_, SSID}] = [{Key, Value} || {Key, Value} <- tests:lt_parse_cookie(Cookies), Key =:= <<"MIJKSSID">> ],
    if is_binary(SSID) ->
            case mijk_session:mcache_check_session_data(SSID) of
                {ok, Data} when is_binary(Data) ->
                    EData = mijk_session:mysql_to_erl(Data),
                    NewData = case proplists:get_value(<<"counter">>, EData, undefined) of
                        undefined            -> [{<<"counter">>, 1}] ++ EData;
                        V when is_integer(V) -> lists:keyreplace(<<"counter">>, 1, EData, {<<"counter">>, V+1})
                    end,
                    mijk_session:mcache_update_session(SSID, mijkweb_utils:merge_plists(SessionData, NewData)),
                    [SSID, [cowboy_cookies:cookie(<<"MIJKSSID">>, SSID,[{max_age, ?SESSION_AGE},{path, <<"/">>}])]]
                ;_ ->
                    [SSID, [cowboy_cookies:cookie(<<"MIJKSSID">>, mijk_session:mcache_create_session(1),[{max_age, ?SESSION_AGE},{path, <<"/">>}])]]
            end;
       true            ->
            NSSID = mijk_session:mcache_create_session(1),
            [NSSID, [cowboy_cookies:cookie(<<"MIJKSSID">>, NSSID, [{max_age, ?SESSION_AGE},{path, <<"/">>}])]]
    end;

session_process(ellic, Req, SessionData) ->
    %lager:debug("MWS1ic ~p", [SessionData]),
    Cookies = elli_request:get_header(<<"Cookie">>, Req, []),
    %lager:debug("ElliPOLLc req2-1: ~p ~n", [Cookies]),
    [Key, Data, _Sign] = mijkweb_utils:plist_values(tests:lt_parse_cookie(Cookies),
        [{<<"MIJKSSID">>, undefined}, {<<"MIJKSSD">>, <<"e30%3D">>}, {<<"MIJKSS">>, <<"e30%3D">>}]),
    {EData} = jiffy:decode(mijkweb_utils:cookie_decode(Data)),
    SData = case proplists:get_value(<<"counter">>, EData, undefined) of
        undefined            -> [{<<"counter">>, 1}] ++ EData;
        V when is_integer(V) -> lists:keyreplace(<<"counter">>, 1, EData, {<<"counter">>, V+1})
    end,
    %lager:debug("SPc1 ~p", [SData]),
    NewData = mijkweb_utils:merge_plists(SData, 
        mijkweb_utils:merge_plists(SessionData, [{<<"ts">>, mijkweb_utils:now()}])),
    %lager:debug("SPc2", []),
    JNewData = jiffy:encode({NewData}),
    %lager:debug("SPc3", []),
    SJNewData = auth_utils:sign(JNewData),
    %lager:debug("SPc4", []),
    NSSID = case is_binary(Key) of
        true -> Key
        ;_   -> mijk_session:generate_session_uuid(1)
    end,
    %lager:debug("SPc5", []),
    [NSSID, [
        cowboy_cookies:cookie(<<"MIJKSSID">>, NSSID,     [{max_age, ?SESSION_AGE},{path, <<"/">>}]),
        %cowboy_cookies:cookie(<<"MIJKSSD">>,  base64:encode(JNewData),  [{max_age, ?SESSION_AGE},{path, <<"/">>}]),
        cowboy_cookies:cookie(<<"MIJKSSD">>,  mijkweb_utils:cookie_encode(JNewData),  [{max_age, ?SESSION_AGE},{path, <<"/">>}]),
        cowboy_cookies:cookie(<<"MIJKSS">>,   SJNewData, [{max_age, ?SESSION_AGE},{path, <<"/">>}])
    ]].

%-spec session_process(atom(), any(), proplist(), any()) -> [binary, list()].
session_process(ellimcd, _Req, SessionData, SSID0) ->
    lager:debug("SPMCD1 ~p", [SSID0]),
    SSID = case SSID0 of
        {<<"">>, <<"auth">>} -> mijk_session:mcd_create_session(1);
        {OSSID, <<"auth">>} when is_binary(OSSID) ->
            lager:warning("auth with old session: ~p", [_Req]), OSSID
        ;_                   -> SSID0
    end,
    if is_binary(SSID) ->
            case mijk_session:mcd_check_session_data(SSID) of
                {ok, Data} when is_binary(Data) ->
                    EData = mijk_session:mysql_to_erl(Data),
                    lager:debug("SPMCD2 ~p", [EData]),
                    NewData = case proplists:get_value(<<"counter">>, EData, undefined) of
                        undefined            -> [{<<"counter">>, 1}] ++ EData;
                        V when is_integer(V) -> lists:keyreplace(<<"counter">>, 1, EData, {<<"counter">>, V+1})
                    end,
                    lager:debug("SPMCD3 ~p", [NewData]),
                    mijk_session:mcd_update_session(SSID, mijkweb_utils:merge_plists(SessionData, NewData)),
                    [SSID, [cowboy_cookies:cookie(<<"MIJKSSID">>, SSID,[{max_age, ?SESSION_AGE},{path, <<"/">>}])]]
                ;_ ->
                    NSSID = mijk_session:mcd_create_session(1),
                    [NSSID, [cowboy_cookies:cookie(<<"MIJKSSID">>, NSSID, [{max_age, ?SESSION_AGE},{path, <<"/">>}])]]
            end;
       true            ->
            NSSID = mijk_session:mcd_create_session(1),
            [NSSID, [cowboy_cookies:cookie(<<"MIJKSSID">>, NSSID, [{max_age, ?SESSION_AGE},{path, <<"/">>}])]]
    end.

-spec check_auth(atom(), any()) -> {ok, any()}.
check_auth(elli, Req) -> 
    Cookies = elli_request:get_header(<<"Cookie">>, Req, []),
    [_Key, Data, Sign] = mijkweb_utils:plist_values(tests:lt_parse_cookie(Cookies),
        [{<<"MIJKSSID">>, undefined}, {<<"MIJKSSD">>, <<"e30=">>}, {<<"MIJKSS">>, <<"e30=">>}]),
    DData = base64:decode(Data),
    case auth_utils:check_sign(DData, Sign) of
        true -> check_session_data(DData)
        ;_   -> false
    end;

check_auth(ellimcd, Req) -> 

    [SSID] = mijkweb_utils:plist_values(tests:lt_parse_cookie(
        elli_request:get_header(<<"Cookie">>, Req, [])),
        [{<<"MIJKSSID">>, undefined}]),
    lager:debug("MSCA: ~p", [SSID]),
    check_session_data(ellimcd, SSID).

-spec check_session_data(binary()) -> false | {ok, list()}.
check_session_data(Data) ->
    SData = jiffy:decode(Data),
    case proplists:get_value(<<"ts">>, SData, undefined) of
        TS when is_integer(TS), TS > 0 ->
            case (mijkweb_utils:now() - TS) > mijkweb_utils:session_age()  of
                true -> {ok, SData}
                ;_   -> false
            end
        ;_ -> false
    end.

check_session_data(ellimcd, SSID) ->
    lager:debug("MSCSD: ~p", [SSID]),
    case mijk_session:mcd_check_session_data(SSID) of
        {ok, Data} when is_binary(Data) ->
            lager:debug("MSCSD1--: ~p", [Data]),
            case mijkweb_session:check_session(mijk_session:mysql_to_erl(Data)) of
                {ok, NewData}   ->
                    lager:debug("MSCSD2--: ~p", [NewData]),
                    mijk_session:mcd_update_session(SSID, NewData),
                    lager:debug("MSCSD3--:", []),
                    {ok, [SSID, [cowboy_cookies:cookie(<<"MIJKSSID">>, SSID,
                        [{max_age, mijkweb_utils:session_age()},{path, <<"/">>}])], NewData]};
                {error, Reason} ->
                    lager:error("CSSD error reason ~p", [Reason]),
                    [];
                _               ->
                    lager:error("CSSD unknown error", []),
                    []
            end
        ;_ ->
            lager:debug("MSCSD2:", []),
            lager:error("CSSD no session data", []),
            []
    end.
   
-spec check_session(list()) -> {ok|error, any()}. 
check_session(Session)      ->
    [AccountId, TimeStamp, LoginType] = mijkweb_utils:plist_values(Session,
        [{<<"accountid">>, undefined}, {<<"timestamp">>, undefined}, {<<"logintype">>, undefined}]),
    lager:debug("!MSCS1: ~p ~p ~p ~n ~p", [AccountId, TimeStamp, LoginType, Session]),
    case {AccountId, TimeStamp, LoginType} of
        {undefined, _, _} -> {error, <<"no account_id">>};
        {_, _, undefined} -> {error, <<"unknown login type">>};
        {_, undefined, _} -> {error, <<"session timestamp undefined">>};
        {Aid, Tstmp, Lt } when is_integer(Aid), is_integer(Tstmp), is_integer(Lt)
                          ->
            NowTs = mijkweb_utils:now(),
            lager:debug("!MSCS2: ~p", [NowTs]),
            case NowTs - Tstmp > mijkweb_utils:session_age() of
                true      -> {error, <<"session timestamp expired">>};
                _         -> {ok, mijkweb_utils:merge_plists(Session, [{<<"timestamp">>, NowTs}])}
            end;
        _                 -> {error, <<"unknown error">>}
    end.

clean_session(ellimcd, Req) ->
    [SSID] = mijkweb_utils:plist_values(tests:lt_parse_cookie(
        elli_request:get_header(<<"Cookie">>, Req, [])), [{<<"MIJKSSID">>, <<"">>}]),
    mijk_session:mcd_delete_session(SSID),
    lager:debug("CS: prepare for CH generation ~p", [SSID]),
    CH = cowboy_cookies:cookie(<<"MIJKSSID">>, SSID, [{max_age, 0},{path, <<"/">>}]),
    lager:debug("CS: CH: ~p", [CH]),
    CH.
