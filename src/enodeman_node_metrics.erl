-module(enodeman_node_metrics).

-export([
    get_descr/0,
    all_metrics/0
]).

%{
%    id,    % user can refer to it via this id
%    title, % description
%    update_info, % {module, fun, [args]}
%    extended_info    % is it possible to get extended info at /ext_info?metric=%s
%}).

%{io, {erlang, statistics, [io]}},
%{garbage_collection, {erlang, statistics, [garbage_collection]}},
%{"memory.total", },
%{ports, {erlang, ports, []}}, 
%{"process_count", {erlang, system_info, [process_count]}},
%{reductions, {erlang, statistics, [reductions]}},
%{runtime, {erlang, statistics, [runtime]}},
%{wordsize, {erlang, system_info, [wordsize]}}
all_metrics_ll() -> [
    { 
        memory.total,
        <<"Total amount of memory allocated">>,
        {erlang, memory, [total]},
        true
    },
    {
        process_count,
        <<"Number of processes on the node">>,
        {erlang, system_info, [process_count]},
        false
    },
    {
        reductions,
        <<"Total reductions number">>,
        {enodeman_remote_module, reductions, []},
        false
    },
    {
        'io.input',
        <<"Total incoming io">>,
        {enodeman_remote_module, io, [1]},
        false
    },
    {
        'io.output',
        <<"Total outgoing io">>,
        {enodeman_remote_module, io, [2]},
        false
    },
    {
        uptime,
        <<"Total node uptime">>,
        {enodeman_remote_module, uptime, []},
        false
    }
].

all_metrics() -> 
    [ {I,U} || {I,_,U,_} <- all_metrics_ll() ].

get_descr() -> 
    [ 
        % hack: api should be always false for id=node (js + clientside)
        [{id, node},{title, <<"Node name">>}, {api, false}] | 
        [ 
            [{id, I},{title, T},{api,A}] 
            || {I,T,_,A} <- all_metrics_ll()
        ]
    ].
