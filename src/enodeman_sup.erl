-module(enodeman_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(M, Fun, Type), {M, {M, Fun, []}, permanent, 5000, Type, [M]}).
-define(CHILD(M, F, A, Type), {M, {M, F, A}, permanent, 5000, Type, [M]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RiakOptions = [
        {connection_options, [
            {host, enodeman_util:get_env(riak_host)},
            {port, enodeman_util:get_env(riak_port)}
        ]}
    ],
    Riak = ?CHILD(simple_riak_pool, start_link, [RiakOptions], worker),
    Stats = ?CHILD(enodeman_stats_collector, worker),
    Nodes = ?CHILD(enodeman_nodes, worker),
    Web = ?CHILD(enodeman_web, start, worker),
    {ok, { {one_for_one, 5, 10}, [Riak, Stats, Nodes, Web]} }.
