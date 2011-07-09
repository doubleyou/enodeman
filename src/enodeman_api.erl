-module(enodeman_api).
-export([
    connect/2
]).

connect(Node, Params) ->
    Cookie = list_to_atom(proplists:get_value("cookie", Params, "")),
    _Pid = enodeman_nodes:node_to_pid(Node, Cookie),
    %% TODO: gather some basic data here
    "ok".
