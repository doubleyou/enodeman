-module(enodeman_node_controller).
-behaviour(gen_server).
-export([
    start_link/2
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(state, {
    node,
    cookie,
    memory,
    runtime,
    io,
    reductions,
    processes,
    ports
}).

start_link(NodeString, Cookie) ->
    gen_server:start_link(?MODULE, [NodeString, Cookie], []).

init([NodeString, Cookie]) ->
    Node = list_to_atom(NodeString),
    erlang:set_cookie(node(), Cookie),
    true = net_kernel:connect_node(Node),
    State = #state{
        node = Node,
        cookie = Cookie
    },
    {ok, renew_stats(State)}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(renew_stats, State) ->
    {noreply, renew_stats(State)};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

renew_stats(State = #state{ node = Node }) ->
    NewState = State#state{
        memory = rpc:call(Node, erlang, memory, []),
        runtime = rpc:call(Node, erlang, statistics, [runtime]),
        reductions = rpc:call(Node, erlang, statistics, [reductions]),
        io = rpc:call(Node, erlang, statistics, [io]),
        processes = rpc:call(Node, erlang, processes, []),
        ports = rpc:call(Node, erlang, ports, [])
    },
    erlang:send_after(1000, self(), renew_stats),
    NewState.
