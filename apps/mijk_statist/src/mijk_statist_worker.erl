-module(mijk_statist_worker).
-behaviour(gen_server).

-export([start_link/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {wait_list=[]}).
-define(SESSION_AGE, 300).

start_link(Args) -> gen_server:start_link(?MODULE, Args, []).
start_link()     -> gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    {ok, #state{wait_list=[]}}.

handle_call({get_user_stats, AccountID}, _From, State) ->
    AIDb = list_to_binary(integer_to_list(AccountID)),
    Ret = case mcd:get(<<"sysacc_stat_", AIDb/binary>>) of
        {ok, B} when is_binary(B) -> {ok, B}
        ;_                        -> {error, notfound}
    end,
    {reply, Ret, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({wait_list_update, AccountID, BinaryTerm}, #state{wait_list = WL} = State) ->
    NWL = case lists:member(AccountID, WL) of
        true  -> WL;
        false -> lists:append(AccountID, WL)
    end,
    AIDb = list_to_binary(integer_to_list(AccountID)),
    mcd:ldo(set, <<"sysacc_stat_", AIDb/binary>>, BinaryTerm, 0, ?SESSION_AGE),
    {noreply, State#state{wait_list = NWL}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
