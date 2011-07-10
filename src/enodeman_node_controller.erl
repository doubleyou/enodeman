-module(enodeman_node_controller).
-behaviour(gen_server).
-export([
    start_link/3,
    node_status/2,
    node_processes/1,
    node_processes_grid/1,
    status/1,
    node_name/1
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

%%%%%%%%%%%%%%%%%%%      API       %%%%%%%%%%%%%%%%%%%%%%%%

node_status(Pid, _Params) -> 
    gen_server:call(Pid, node_status).

node_processes(Pid) -> 
    gen_server:call(Pid, node_processes).

start_link(Parent, NodeString, Cookie) ->
    gen_server:start_link(?MODULE, [Parent, NodeString, Cookie], []).

node_name(Pid) ->
    gen_server:call(Pid, node_name).

status(Pid) ->
    gen_server:call(Pid, status).

%%%%%%%%%%%%%%%%%%%% gen_server callback functions %%%%%%%%%%%%%%

init([Parent, NodeString, Cookie]) ->
    %enodeman_util:info(?MODULE, "init(~p)~n", [[Parent, NodeString, Cookie]]),
    Node = list_to_atom(NodeString),
    erlang:set_cookie(node(), Cookie),
    true = net_kernel:hidden_connect(Node),

    Timestamp = enodeman_util:now(),
    Interval = enodeman_util:get_env(node_metrics_update_interval),
    enodeman_stats_collector:new_source({node, Node}, Timestamp, Interval, []),
    case reload_remote_module(Node) of
        {badrpc, _} = Error ->
            enodeman_util:err(?MODULE, "can't reload remote module:~n~p~n", [Error]),
            {stop, Error};
        _ ->
            State = #state{ parent = Parent, node = Node, cookie = Cookie },
            %TODO :redesign
            case renew_procs(State) of
                {ok, State1} -> 
                    case renew_metrics(State1) of
                        {ok, State2} -> {ok, State2};
                        Error -> {stop, Error}
                    end;
                Error -> {stop, Error}
            end
    end.

handle_call(status, _From, State) ->
    {reply, State, State};
handle_call(node_status, _From, #state{metrics=Metrics} = State) ->
    {reply, Metrics, State};
handle_call(node_processes, _From, #state{processes=P} = State) ->
    {reply, P, State};
handle_call(node_name, _From, #state{node=N} = State) ->
    {reply, N, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(renew_metrics, State) ->
    case renew_metrics(State) of
        {ok, State1} -> {noreply, State1};
        Error -> {stop, Error, State}
    end;
handle_info(renew_procs, State) ->
    case renew_procs(State) of
        {ok, State1} -> {noreply, State1};
        Error -> {stop, Error, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{node=Node}) ->
    enodeman_stats_collector:remove_source({node, Node}),
    ok.

%%%%%%%%%%%%%%%%%%% Internal functions %%%%%%%%%%%%%%%%

renew_metrics(#state{node = Node} = State) ->
    Interval = enodeman_util:get_env(node_metrics_update_interval),
    erlang:send_after(Interval, self(), renew_metrics),
    AllMetrics = enodeman_node_metrics:all_metrics(),
    UserMetrics = enodeman_util:get_env(node_metrics),

    ToCheck = [{K,V} || {K,V} <- AllMetrics, lists:member(K, UserMetrics) ],
    Updates = [{K, update_metric(Node, P)} || {K, P} <- ToCheck],

    enodeman_stats_collector:update({node, Node}, Updates),
    {ok, State#state{ metrics = Updates }}.

renew_procs(#state{node = Node} = State) ->
    Interval = enodeman_util:get_env(processes_update_interval),
    erlang:send_after(Interval, self(), renew_procs),
    Types = enodeman_proc_metrics:get_names(),
    case rpc:call(Node, enodeman_remote_module, get_processes_info, [Types]) of
        {badrpc, _} = Error -> Error;
        P -> 
            {ok, State#state{processes = post_process_procs(Node, P)}}
    end.

post_process_procs(Node, Procs) ->
    [ 
        begin
                Pid = list_to_binary(pid_to_list(P)),
                UpdatedMs = [{pid, Pid} | Ms],
                Metrics = [process_proc_metric(Node, P, M) || M <- UpdatedMs],
                {Pid, Metrics}
        end || {P, Ms} <- Procs
    ].

process_proc_metric(Node, Pid, {initial_call, {proc_lib, init_p, 5}}) ->
    {dictionary, Dict} = rpc:call(Node, erlang, process_info, [Pid, dictionary]),
    ActualInitialCall = proplists:get_value('$initial_call', Dict),
    process_proc_metric(Node, Pid, {initial_call, ActualInitialCall});
process_proc_metric(_, _, {initial_call, {M, F, Arity}}) ->
    {initial_call, list_to_binary(
        atom_to_list(M) ++ ":" ++ 
        atom_to_list(F) ++ "/" ++ 
        integer_to_list(Arity))};
process_proc_metric(_, _, V) -> V.

update_metric(Node, {M, F, A}) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, Reason} ->
            enodeman_util:err(?MODULE,
                "error while doing rpc:call(~p,~p,~p,~p) ->~n~p~n",
                [Node, M, F, A, Reason]
            ),
            undefined;
        V ->
            V
    end.

reload_remote_module(Node) ->
    c:l(enodeman_remote_module),
    {ok, B} = file:read_file(code:which(enodeman_remote_module)),
    rpc:call(Node, code, load_binary, 
        [enodeman_remote_module, "enodeman_remote_module.erl", B]).

node_processes_grid(Pid) ->
    Processes = node_processes(Pid),
    _Need = [
                {<<"page">>,1},
                {<<"total">>,1},
                {<<"records">>,2},
                {<<"rows">>,[
                        {struct, [
                                {<<"id">>,process_name_1},
                                {<<"cell">>, [process_name_1, <<"aaa:bbb/2">>, <<"10000">>, <<"10000">>, <<"2">>]}
                            ]},
                        {struct, [
                                {<<"id">>,process_name_2},
                                {<<"cell">>, [process_name_2, <<"bbb:ccc/2">>, <<"10000">>, <<"10000">>, <<"2">>]}
                            ]}
                    ]
                }
            ],
    [
        {<<"page">>,1},
        {<<"total">>,1},
        {<<"records">>,2},
        {<<"rows">>, [process_info_to_row_grid(P) || P <- Processes]}
    ].

process_info_to_row_grid({Pid, PL}) -> 
    {struct, [
            {<<"id">>, Pid},
            {<<"cell">>, [V || {_,V} <- PL]}
        ]
    }.
