-module(enodeman_node_sup).
-behaviour(supervisor).
-export([
    start_link/2,
    start_child/3
]).
-export([
    init/1
]).

start_link(Node, Cookie) ->
    supervisor:start_link(?MODULE, [Node, Cookie]).

start_child(Pid, Mod, Args) ->
    supervisor:start_child(
        Pid,
        {Mod, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}
    ).

init([Node, Cookie]) ->
    Self = self(),
    Controller = {
        enodeman_node_controller,
        {enodeman_node_controller, start_link, [Self, Node, Cookie]},
        permanent, 5000, worker, [enodeman_node_controller]
    },
    {ok, { {one_for_one, 5, 10}, [Controller]}}.
