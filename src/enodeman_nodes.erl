-module(enodeman_nodes).
-behaviour(supervisor).
-export([
    start_link/0,
    node_to_pid/1,
    node_to_pid/2,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

node_to_pid(Node) ->
    {_, Pid, _, _} = lists:keyfind(Node, 1, supervisor:which_children(?MODULE)),
    Pid.

node_to_pid(Node, Cookie) when is_atom(Node) ->
    node_to_pid(atom_to_list(Node), Cookie);
node_to_pid(Node, Cookie) ->
    case lists:keyfind(Node, 1, supervisor:which_children(?MODULE)) of
        {_, Pid, _, _} ->
            Pid;
        false ->
            {ok, ParentPid} = supervisor:start_child(
                ?MODULE,
                {
                    Node,
                    {enodeman_node_sup, start_link, [Node, Cookie]},
                    permanent, infinity, supervisor, [enodeman_node_sup]
                }),
            Specs = supervisor:which_children(ParentPid),
            {_, Pid, _, _} = lists:keyfind(enodeman_node_controller, 1, Specs),
            Pid
    end.

init(_) ->
    {ok, { {one_for_one, 5, 10}, []}}.
