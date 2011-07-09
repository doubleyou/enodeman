-module(enodeman_api).
-export([
    connect/2,
    node_status/2
]).

connect(Node, Params) ->
    Cookie = list_to_atom(proplists:get_value("cookie", Params, "")),
    enodeman_nodes:node_to_pid(Node, Cookie),
    <<"ok">>.

node_status(Pid, Params) ->
    <<"ok">>.
