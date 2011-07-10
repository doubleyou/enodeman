-module(enodeman_node_metrics).

-export([
    get_descr/0,
    all_metrics/0,
    all_metrics_ll/0
]).

%{
%    id,    % user can refer to it via this id
%    title, % description
%    update_info, % {module, fun, [args]}
%    api          % more info on  http://<URL>/<NODE>/<api>
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
        <<"/memory">>
    },
    {
        process_count,
        <<"Number of processes on the node">>,
        {erlang, memory, [total]},
        undefined
    }
].

all_metrics() -> 
    [ {I,U} || {I,_,U,_} <- all_metrics_ll() ].

get_descr() -> 
    [ 
        [{id, I},{title, T},{api,A}] 
        || {I,T,_,A} <- all_metrics_ll()
    ].
