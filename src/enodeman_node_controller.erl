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
    metrics = []
}).

repl_metrics_values(Values, #state{metrics=M}) ->
    lists:foldl(
        fun ({K,V}, Acc) -> lists:keystore(K, 1, Acc, {K,V});
            (_, Acc) -> Acc
        end, M, Values).

get_metric(K, #state{metrics=M}) -> 
    {K, V} = lists:keyfind(K, 1, M),
    V.

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

update_metric({Node, M, F, A}, OldValue) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, Reason} -> enodeman_utils:warning(
                "error while doing rpc:call(~p,~p,~p,~p) ->~n~p~n",
                [Node, M, F, A, Reason]),
            OldValue;
        V -> V
    end.

renew_stats(#state{node = Node} = State) ->
    erlang:send_after(1000, self(), renew_stats),
    Params = [
        {memory, {Node, erlang, memory, []}},
        {runtime, {Node, erlang, statistics, [runtime]}},
        {reductions, {Node, erlang, statistics, [reductions]}},
        {io, {Node, erlang, statistics, [io]}},
        %TODO: more clever get proc list
        {processes, {Node, erlang, processes, []}},
        {ports, {Node, erlang, ports, []}}],
    Updates = [ {K, update_metric(P, get_metric(K, State))} || {K, P} <- Params ],
    repl_metrics_values(Updates, State).
