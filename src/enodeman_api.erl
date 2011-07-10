-module(enodeman_api).
-export([
    connect/2,
    node_status/2,
    stats/2,
    node_metrics/0,
    proc_metrics/0
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

stats(Node, Params) ->
    Date = case [lists:keyfind(K, Params) || K<-["year","month","day"]] of
        [false, false, false] -> date();
        L -> list_to_tuple(lists:map(fun list_to_integer/1, L))
    end,
    [
        begin
            Segments = enodeman_stats_collector:read_day({node, Node}, M, Date),
            PointsPerSegment = 20 div length(Segments),
            [reduce_stats(PointsPerSegment, Stats) || Stats <- Segments]
        end
        || {M, _} <- enodeman_node_metrics:all_metrics()
    ].

reduce_stats(MaxPoints, {StartTime, Interval, Stats}) ->
    Lists = split_by(MaxPoints, Stats),
    NewInterval = Interval * length(Stats) / length(Lists),
    {StartTime, NewInterval, lists:map(fun avg_quad/1, Lists)}.

split_by(N, L) when length(L) > N ->
    {Pref, Suff} = lists:split(N, L),
    [Pref | split_by(N, Suff)];
split_by(_, L) ->
    L.

avg_quad(L) ->
    Total = length(L),
    math:sqrt(lists:foldl(
        fun(I, Acc) ->
            Acc + I*I
        end,
        0,
        L
    ) / (Total) * (Total - 1)).
