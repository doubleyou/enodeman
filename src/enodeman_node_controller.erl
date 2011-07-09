-module(enodeman_node_controller).
-behaviour(gen_server).
-export([
    start_link/3,
    node_status/2
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
    metrics = [],
    processes = []
}).

-define(NODE_METRICS_UPDATE_INTERVAL, 1000).
-define(PROCESSES_UPDATE_INTERVAL, 1000).

%%%%%%%%%%%%%%%%%%%      API       %%%%%%%%%%%%%%%%%%%%%%%%

node_status(Pid, _Params) -> 
    gen_server:call(Pid, node_status).

start_link(Parent, NodeString, Cookie) ->
    gen_server:start_link(?MODULE, [Parent, NodeString, Cookie], []).

%%%%%%%%%%%%%%%%%%%% gen_server callback functions %%%%%%%%%%%%%%

init([Parent, NodeString, Cookie]) ->
    Node = list_to_atom(NodeString),
    erlang:set_cookie(node(), Cookie),
    true = net_kernel:connect_node(Node),
    error_logger:format("~p:init Node:~p~n", [?MODULE, Node]),

    State = #state{
        parent = Parent,
        node = Node,
        cookie = Cookie
    },
    {ok, renew_metrics(renew_procs(State))}.

handle_call(status, _From, State) ->
    {reply, State, State};
handle_call(node_status, _From, #state{metrics=Metrics} = State) ->
    {reply, Metrics, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(renew_metrics, State) ->
    {noreply, renew_metrics(State)};
handle_info(renew_procs, State) ->
    {noreply, renew_procs(State)};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%% Internal functions %%%%%%%%%%%%%%%%

renew_metrics(#state{node = Node} = State) ->
    Interval = enodeman_util:get_env(node_metrics_update_interval),
    erlang:send_after(Interval, self(), renew_metrics),
    AllMetrics = enodeman_node_metrics:all_metrics(),
    UserMetrics = enodeman_util:get_env(node_metrics),

    ToCheck = [{K,V} || {K,V} <- AllMetrics, lists:member(K, UserMetrics) ],
    Updates = [{K, update_metric(Node, P)} || {K, P} <- ToCheck],
    State#state{ metrics = Updates }.

renew_procs(#state{node = _Node, processes = _Ps} = State) ->
    Interval = enodeman_util:get_env(processes_update_interval),
    erlang:send_after(Interval, self(), renew_procs),
    State.

update_metric(Node, {M, F, A}) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, Reason} ->
            enodeman_utils:warning(
                "error while doing rpc:call(~p,~p,~p,~p) ->~n~p~n",
                [Node, M, F, A, Reason]
            ),
            undefined;
        V ->
            V
    end.
