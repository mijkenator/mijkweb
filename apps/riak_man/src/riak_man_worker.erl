-module(riak_man_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    Hostname = proplists:get_value(hostname, Args),
    Port     = proplists:get_value(port, Args),
    {ok, Pid} = riakc_pb_socket:start_link(Hostname, Port),
    {ok, #state{conn=Pid}}.

handle_call({ping}, _From, #state{conn=Conn}=State) ->
    {reply, riakc_pb_socket:ping(Conn), State};
handle_call({list_keys, Bucket}, _From, #state{conn=Conn}=State) ->
    {reply, riakc_pb_socket:list_keys(Conn, Bucket), State};
handle_call({list_buckets, Params}, _From, #state{conn=Conn}=State) ->
    Ret = erlang:apply(riakc_pb_socket, list_buckets, [Conn | Params]),
    {reply, Ret, State};
handle_call({store, Bucket, Key, Body, BParams}, _From, #state{conn=Conn}=State) ->
    %io:format("Store1 ~n", []),
    Object = riakc_obj:new(Bucket, Key, Body),
    %io:format("Store2 ~p ~n", [Object]),
    Ret = riakc_pb_socket:put(Conn, Object, BParams),
    {reply, Ret, State};
handle_call({put, Object, BParams}, _From, #state{conn=Conn}=State) ->
    Ret = riakc_pb_socket:put(Conn, Object, BParams),
    {reply, Ret, State};
handle_call({method, Method, Params}, _From, #state{conn=Conn}=State) ->
    Ret = erlang:apply(riakc_pb_socket, Method, [Conn | Params]),
    {reply, Ret, State};
handle_call({get, Bucket, Key}, _From, #state{conn=Conn}=State) ->
    Ret = riakc_pb_socket:get(Conn, Bucket, Key),
    {reply, Ret, State};
handle_call({get, Bucket, Key, Timeout, Options}, _,  #state{conn=Conn}=State) ->
    Ret = riakc_pb_socket:get(Conn, Bucket, Key, Options, Timeout),
    {reply, Ret, State};
handle_call({get_value, Bucket, Key, Timeout, Options}, _, #state{conn=Conn}=State) ->
    case riakc_pb_socket:get(Conn, Bucket, Key, Options, Timeout) of
        {ok, Obj} ->  {reply, riakc_obj:get_value(Obj), State};
        _         ->  {reply, undefined, State}
    end;
handle_call({get_value, Bucket, Key}, _, #state{conn=Conn}=State) ->
    case riakc_pb_socket:get(Conn, Bucket, Key) of
        {ok, Obj} ->  {reply, riakc_obj:get_value(Obj), State};
        _         ->  {reply, undefined, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=_Conn}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
