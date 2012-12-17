-module(mijkweb_session).

-export([
    session_process/2,
    session_process/3,
    check_auth/2
]).

-include("include/consts.hrl").

%-spec session_process(atom(), any()) -> [binary(), list()].
session_process(elli, Req) -> session_process(elli, Req, []).

%-spec session_process(atom(), any(), proplist()) -> [binary, list()].
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

-spec check_auth(atom(), any()) -> {ok, any()}.
check_auth(elli, Req) -> 
    Cookies = elli_request:get_header(<<"Cookie">>, Req, []),
    [_Key, Data, Sign] = mijkweb_utils:plist_values(tests:lt_parse_cookie(Cookies),
        [{<<"MIJKSSID">>, undefined}, {<<"MIJKSSD">>, <<"e30=">>}, {<<"MIJKSS">>, <<"e30=">>}]),
    DData = base64:decode(Data),
    case auth_utils:check_sign(DData, Sign) of
        true -> check_session_data(DData)
        ;_   -> false
    end.

-spec check_session_data(binary()) -> false | {ok, list()}.
check_session_data(Data) ->
    SData = jiffy:decode(Data),
    case proplists:get_value(<<"ts">>, SData, undefined) of
        TS when is_integer(TS), TS > 0 ->
            case (mijkweb_utils:now() - TS) > ?SESSION_AGE  of
                true -> {ok, SData}
                ;_   -> false
            end
        ;_ -> false
    end.


