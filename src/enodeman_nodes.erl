-module(enodeman_nodes).
-behaviour(supervisor).
-export([
    start_link/0,
    node_to_pid/1,
    connect/2,
    connect_siblings/2,
    which_nodes/0,
    init/1
]).

start_link() ->
    P = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    DefaultNodes = enodeman_util:get_env(default_nodes),
    [
        begin
            connect(Node, Cookie),
            connect_siblings(Node, Cookie)
        end
        || {Node, Cookie} <- DefaultNodes
    ],
    P.

which_nodes() ->
    [Id || {Id, _, _, _} <- supervisor:which_children(?MODULE)].

node_to_pid(Node) when is_atom(Node) -> node_to_pid(atom_to_list(Node));
node_to_pid(Node) ->
    {_, ParentPid, _, _} = lists:keyfind(Node, 1, supervisor:which_children(?MODULE)),
    controller_by_sup(ParentPid).

connect(Node, Cookie) when is_atom(Node) ->
    connect(atom_to_list(Node), Cookie);
connect(Node, Cookie) ->
    case lists:keyfind(Node, 1, supervisor:which_children(?MODULE)) of
        {_, Pid, _, _} ->
            controller_by_sup(Pid);
        false ->
            {ok, ParentPid} = supervisor:start_child(
                ?MODULE,
                {
                    Node,
                    {enodeman_node_sup, start_link, [Node, Cookie]},
                    permanent, infinity, supervisor, [enodeman_node_sup]
                }),
            controller_by_sup(ParentPid)
    end.

connect_siblings(Node, Cookie) when is_list(Node) ->
    connect_siblings(list_to_atom(Node), Cookie);
connect_siblings(Node, Cookie) ->
    Nodes = rpc:call(Node, erlang, nodes, [connected]),
    [catch enodeman_nodes:connect(N, Cookie) || N <- Nodes].

controller_by_sup(ParentPid) ->
    Specs = supervisor:which_children(ParentPid),
    {_, Pid, _, _} = lists:keyfind(enodeman_node_controller, 1, Specs),
    Pid.

init(_) ->
    {ok, { {one_for_one, 5, 10}, []}}.
