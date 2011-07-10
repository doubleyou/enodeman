-module(enodeman_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(M, Fun, Type), {M, {M, Fun, []}, permanent, 5000, Type, [M]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Riak = ?CHILD(simple_riak_pool, worker),
    Stats = ?CHILD(enodeman_stats_collector, worker),
    Nodes = ?CHILD(enodeman_nodes, worker),
    Web = ?CHILD(enodeman_web, start, worker),
    {ok, { {one_for_one, 5, 10}, [Riak, Stats, Nodes, Web]} }.
