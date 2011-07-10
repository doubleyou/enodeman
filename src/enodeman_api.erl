-module(enodeman_api).
-export([
    connect/2,
    node_status/2,
    node_metrics/0,
    proc_metrics/0,
    processes_raw/2
]).

%  :8080/<NODE>/<FUNC>?a=b&c=d
%  http://127.0.0.1:8080/enodeman@127.0.0.1/processes_list?a=b&c=d

connect(Node, Params) ->
    Cookie = list_to_atom(proplists:get_value("cookie", Params, "")),
    enodeman_nodes:node_to_pid(Node, Cookie),
    <<"ok">>.

node_status(Pid, Params) ->
    enodeman_node_controller:node_status(Pid, Params).

node_metrics() ->
    enodeman_node_metrics:get_descr().

proc_metrics() ->
    enodeman_proc_metrics:get_descr().
    
processes_raw(Pid, Params) ->
    {struct, enodeman_node_controller:node_processes(Pid, Params)}.
