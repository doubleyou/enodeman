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

new_state(Values) ->
    Default = [
        {"node", undefined},
        {"cookie", undefined},
        {"memory", undefined},
        {"runtime", undefined},
        {"io", undefined},
        {"reductions", undefined},
        {"processes", undefined},
        {ports, undefined}
    ],
    repl_state_values(Values, Default).

repl_state_values(Values, PL) ->
    lists:foldl(
        fun ({K,V}, Acc) -> lists:keystore(K, 1, Acc, {K,V});
            (_, Acc) -> Acc
        end, PL, Values).

get_value(K, L) -> 
    {K, V} = lists:keyfind(K, 1, L),
    V.

start_link(NodeString, Cookie) ->
    gen_server:start_link(?MODULE, [NodeString, Cookie], []).

init([NodeString, Cookie]) ->
    Node = list_to_atom(NodeString),
    erlang:set_cookie(node(), Cookie),
    true = net_kernel:connect_node(Node),
    State = new_state([
        {"node", Node},
        {"cookie", Cookie}
    ]),
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

update_value({Node, M, F, A}, OldValue) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, Reason} -> enodeman_utils:warning(
                "error while doing rpc:call(~p,~p,~p,~p) ->~n~p~n",
                [Node, M, F, A, Reason]),
            OldValue;
        V -> V
    end.

renew_stats(State) ->
    erlang:send_after(1000, self(), renew_stats),
    Node = get_value("node", State),
    Params = [
        {"memory", {Node, erlang, memory, []}},
        {"runtime", {Node, erlang, statistics, [runtime]}},
        {"reductions", {Node, erlang, statistics, [reductions]}},
        {"io", {Node, erlang, statistics, [io]}},
        %TODO: more clever get proc list
        {"processes", {Node, erlang, processes, []}},
        {"ports", {Node, erlang, ports, []}}],
    Updates = [ {K, update_value(P, get_value(K, State))} || {K, P} <- Params ],
    repl_state_values(Updates, State).
