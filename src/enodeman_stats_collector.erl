-module(enodeman_stats_collector).
-behaviour(gen_server).
-export([
    start_link/3,
    store/2
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(state, {
    type,
    name,
    start_time,
    interval,
    stats
}).

start_link({Type, Name}, StartTime, Interval) ->
    gen_server:start_link(?MODULE, [{Type, Name}, StartTime, Interval], []).

store(Pid, Stats) ->
    gen_server:cast(Pid, {store, Stats}).

init([{Type, Name}, StartTime, Interval]) ->
    {ok, #state{
        type = Type, name = Name,
        start_time = StartTime, interval = Interval
    }}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({store, Stats}, State = #state{ stats = [] }) ->
    {noreply, State#state{ stats = Stats }};
handle_cast({store, Stats}, State = #state{ stats = S }) ->
    {noreply, State#state{ stats = update_stats(Stats, S) }};
handle_cast(flush, State = #state{ stats = Stats, interval = Interval,
                    start_time = StartTime, type = Type, name = Name }) ->
    [
        begin
            D = {StartTime, Interval, V},
            Obj = riakc_obj:new(riak_bucket(Type, Name, Metric), riak_key(), D),
            simple_riak_client:do(put, [Obj, [{w, 1}, {dw, 1}], 1000])
        end
        || {Metric, V} <- Stats
    ],
    erlang:garbage_collect(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

update_stats(Update, Stats) ->
    [{K, [proplists:get_value(K, Update) | V]} || {K, V} <- Stats].

riak_bucket(Type, Name, Metric) ->
    [BT, BN, BM] = [ensure_binary(V) || V <- [Type, Name, Metric]],
    <<BT/binary,"_",BN/binary,"_",BM/binary>>.

riak_key() ->
    riak_key(date()).
riak_key(Date = {_Y, _M, _D}) ->
    list_to_binary(
        string:join(
            [integer_to_list(I) || I <- tuple_to_list(Date)],
            "."
        )
    ).

ensure_binary(A) when is_atom(A) ->
    ensure_binary(atom_to_list(A));
ensure_binary(L) when is_list(L) ->
    list_to_binary(L);
ensure_binary(B) when is_binary(B) ->
    B.
