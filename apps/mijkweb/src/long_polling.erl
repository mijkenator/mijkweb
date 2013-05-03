-module(long_polling).

-export([
    poll/2,
    poll_request/3,
    push_request/2
]).

-include("include/mijkweb_consts.hrl").

-spec poll(integer(), [binary()]) -> {ok, integer(), [any()]}.
poll(Ts, Channels) ->
    {ok, LastTS, Messages} = dps:multi_fetch(Channels, Ts, 30000),
    {ok, LastTS, Messages}.

-spec poll_request(binary(), binary(), list()) -> binary().
poll_request(RequestBody, SessionId, SessionData) when is_binary(RequestBody), is_binary(SessionId)->
    lager:debug("LPPR ~n ~p ~p", [SessionId, SessionData]),
    try jiffy:decode(RequestBody) of
        {PList} when is_list(PList) ->
            lager:debug("LPPR1 ~p ~n", [PList]),
            SysAccountId = case proplists:get_value(<<"logintype">>, SessionData, 0) of
                1 -> proplists:get_value(<<"accountid">>, SessionData,  0);
                2 -> proplists:get_value(<<"loginextra">>, SessionData,  0)
            end,
            DefaultChannels = case proplists:get_value(<<"logintype">>, SessionData, 0) of
                1 -> [list_to_binary(io_lib:format("channel_~p",[SysAccountId]))];
                2 -> [list_to_binary(io_lib:format("channel_~p",[SysAccountId])),
                      list_to_binary(io_lib:format("channel_~p_~p", [SysAccountId, 
                                proplists:get_value(<<"accountid">>, SessionData,  0)]))]
            end,
            [Type, OldSeq, Channels] = mijkweb_utils:plist_values(PList, 
                [{<<"type">>, <<"poll">>}, 
                 {<<"seq">>, 0}, 
                 {<<"channels">>, DefaultChannels}]),
            lager:debug("LPPR2 ~p ~n", [{Type, OldSeq, Channels}]),
            _Session = dps_session:find_or_create(SessionId, Channels),
            %lager:debug("LPPR3 ~p ~n", [Session]),
            %{ok, Seq, Messages} = dps_session:fetch(Session, OldSeq),
            {ok, Seq, Messages} = dps:multi_fetch(Channels, OldSeq, 30000),
            lager:debug("LPPR4 ~p ~n", [{Seq, Messages}]),
            Response = {[
                {<<"type">>, Type},
                {<<"status">>, <<"ok">>},
                {<<"seq">>, Seq},
                {<<"channels">>, Channels},
                {<<"data">>, {[{<<"messages">>, Messages}]}}
            ]},
            lager:debug("LPPR4-1 ~p ~n", [Response]),
            JResp = mijkweb_response:json_ok_response(Response),
            lager:debug("LPPR5 ~p ~n", [JResp]),
            JResp
    catch
        _:_ -> mijkweb_response:json_error_response(?INVALID_JSON) 
    end.

-spec push_request(binary(), list()) -> binary().
push_request(RequestBody, SessionData) when is_binary(RequestBody) ->
    lager:debug("LPPUR1 ~p", [RequestBody]),
    try
        {PList} = jiffy:decode(RequestBody),
    lager:debug("LPPUR1-1 ~p", [PList]),
        is_list(PList) orelse throw({error, bad_command}),
        SysAccountId = case proplists:get_value(<<"logintype">>, SessionData, 0) of
            1 -> proplists:get_value(<<"accountid">>, SessionData, 0);
            2 -> proplists:get_value(<<"loginextra">>, SessionData, 0)
        end,
    lager:debug("LPPUR1-2 ~p", [PList]),
        %[Type, Msg, Channel] = [ proplists:get_value(Ch, PList, undefined) 
        %                         || Ch <- [<<"type">>, <<"message">>, <<"channel">>]],
        [Type, Msg, Channel] = mijkweb_utils:plist_values(PList, 
            [{<<"type">>, <<"push">>}, 
             {<<"message">>, undefined}, 
             {<<"channel">>, list_to_binary(io_lib:format("channel_~p",[SysAccountId]))}]),
    lager:debug("LPPUR1-3 ~p", [PList]),
        is_binary(Channel) orelse throw({error, no_channel}),
        is_binary(Msg) orelse throw({error, no_message}),
    lager:debug("LPPUR2 ~p", [{Type, Msg, Channel}]),
        dps:new(Channel),
        dps:publish(Channel, Msg),
    lager:debug("LPPUR3", []),
        Response = {[
            {<<"type">>, Type},
            {<<"status">>, <<"ok">>}
        ]},
    lager:debug("LPPUR4 ~p", [Response]),
        mijkweb_response:json_ok_response(Response)
    catch
        throw:dps_busy                 -> mijkweb_response:json_error_response(?PUSH_BUSY);
        throw:{error, no_message}      -> mijkweb_response:json_error_response(?PUSH_NOMESSAGE);
        throw:{error, bad_command}     -> mijkweb_response:json_error_response(?JSON_BADCOMMAND);
        throw:{error,{_,invalid_json}} -> mijkweb_response:json_error_response(?INVALID_JSON);
        throw:{error,no_channel}       -> mijkweb_response:json_error_response(?PUSH_NOCHANNEL);
        _E:_R                          ->
            lager:error("push unknown error ~p ~p ", [_E, _R]),
            mijkweb_response:json_error_response(?UNKNOWN_ERROR) 
    end.

