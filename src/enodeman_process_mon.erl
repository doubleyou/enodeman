-module(enodeman_process_mon).
-behaviour(gen_server).
-export([
    start_link/2,
    renew_proc_info/2,
    proc_metrics_all/0,
    proc_metrics_brief/0
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
    pid,
    metrics = []
}).

start_link(Node, Pid) ->
    gen_server:start_link(?MODULE, [Node, Pid], []).

init([Node, Pid]) ->
    State = #state{
        node = Node,
        pid = Pid
    },
    {ok, State#state{ metrics = renew_proc_info(State, proc_metrics_all()) }}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

renew_proc_info(#state{ pid = Pid }, Props) ->
    Interval = enodeman_util:get_env(process_metrics_update_interval),
    erlang:send_after(Interval, self(), renew_proc_info),
    Params = lists:filter(fun ({K, _, _}) ->
            lists:member(K, Props)
        end,
        enodeman_util:get_env(procs_metrics_spec)),
    [{K, process_info(Pid, K)} || {K, _, _} <- Params].

proc_metrics_all() ->
    [current_function, initial_call, status, message_queue_len, priority, reductions, memory, backtrace].

proc_metrics_brief() ->
    [initial_call, memory, reductions, message_queue_len].
