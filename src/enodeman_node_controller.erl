-module(enodeman_node_controller).
-behaviour(gen_server).
-export([
    start_link/3
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
    parent,
    node,
    cookie,
    metrics = []
}).

repl_metrics_values(Values, #state{metrics=M}) ->
    lists:foldl(
        fun ({K,V}, Acc) -> lists:keystore(K, 1, Acc, {K,V});
            (_, Acc) -> Acc
        end, M, Values).

start_link(Parent, NodeString, Cookie) ->
    gen_server:start_link(?MODULE, [Parent, NodeString, Cookie], []).

init([Parent, NodeString, Cookie]) ->
    Node = list_to_atom(NodeString),
    erlang:set_cookie(node(), Cookie),
    true = net_kernel:connect_node(Node),

    State = #state{
        parent = Parent,
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

update_metric(Node, {M, F, A}, OldValue) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, Reason} ->
            enodeman_utils:warning(
                "error while doing rpc:call(~p,~p,~p,~p) ->~n~p~n",
                [Node, M, F, A, Reason]
            ),
            OldValue;
        V ->
            V
    end.

renew_stats(#state{node = Node, metrics = M} = State) ->
    erlang:send_after(1000, self(), renew_stats),
    Params = [
        {memory, {erlang, memory, []}},
        {runtime, {erlang, statistics, [runtime]}},
        {reductions, {erlang, statistics, [reductions]}},
        {io, {erlang, statistics, [io]}},
        %TODO: more clever get proc list
        {processes, {erlang, processes, []}},
        {ports, {erlang, ports, []}}],
    Updates = [{K, update_metric(Node, P, proplists:get_value(K, M))}
        || {K, P} <- Params],
    State#state{
        metrics = repl_metrics_values(Updates, State)
    }.
