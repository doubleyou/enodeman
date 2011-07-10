-module(enodeman_stats_collector).
-behaviour(gen_server).
-export([
    start_link/0,
    new_source/4,
    update/2,
    remove_source/1,
    flush/1,
    read_day/3,
    split_by/2,
    get_stats/2
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_source({Type, Name}, StartTime, Interval, Stats) ->
    gen_server:cast(
        ?MODULE,
        {new_source, {Type, Name, StartTime, Interval}, Stats}
    ).

update({Type, Name}, Stats) ->
    gen_server:cast(?MODULE, {update, {Type, Name}, Stats}).

remove_source({Type, Name}) ->
    gen_server:cast(?MODULE, {stop, {Type, Name}}).

flush({Type, Name}) ->
    gen_server:call(?MODULE, {flush, {Type, Name}}).

read_day({Type, Name}, Metric, Date) ->
    OldData = case simple_riak_pool:do(get, [riak_bucket(Type, Name, Metric), riak_key(Date), [{r, 1}]]) of
        {ok, O} -> binary_to_term(riakc_obj:get_value(O));
        {error, notfound} -> []
    end,
    NewData = case date() of
        Date ->
            gen_server:call(?MODULE, {stats, {Type, Name}, Metric});
        _ ->
            []
    end,
    OldData ++ NewData.

init(_) ->
    {ok, []}.

handle_call({stats, Id, Metric}, _From, State) ->
    {StartTime, Interval, Stats} = proplists:get_value(Id, State),
    {
        reply,
        [{
            StartTime,
            Interval,
            lists:reverse(proplists:get_value(Metric, Stats))
        }],
        State
    };
handle_call({flush, Id = {Type, Name}}, _From, State) ->
    {StartTime, Interval, Stats} = proplists:get_value(Id, State),
    [
        begin
            B = riak_bucket(Type, Name, Metric),
            K = riak_key(),
            V = {StartTime, Interval, lists:reverse(Data)},
            O = maybe_append_data(B, K, V),
            simple_riak_pool:do(put, [O, [{w, 2}, {dw, 1}], 1000])
        end
        || {Metric, Data} <- Stats
    ],
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({new_source, {Type, Name, StartTime, Interval}, RawStats}, State) ->
    Stats = [{K, [V]} || {K, V} <- RawStats],
    {noreply, [{{Type, Name}, {StartTime, Interval, Stats}} | State]};
handle_cast({update, Id, Update}, State) ->
    {StartTime, Interval, OldStats} = proplists:get_value(Id, State),
    NewStats = update_stats(Update, OldStats),
    {
        noreply,
        lists:keyreplace(Id, 1, State, {Id, {StartTime, Interval, NewStats}})
    };
handle_cast({stop, Id}, State) ->
    handle_call({flush, Id}, undefined, State),
    {noreply, lists:keydelete(Id, 1, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.



maybe_append_data(B, K, V) ->
    case simple_riak_pool:do(get, [B, K, [{r, 1}]]) of
        {ok, O} ->
            B = riakc_obj:get_value(O),
            riakc_obj:update_value(O, term_to_binary([V | binary_to_term(B)]));
        {error, notfound} ->
            riakc_obj:new(B, K, term_to_binary([V]))
    end.

update_stats(Update, []) ->
    [{K, [V]} || {K, V} <- Update];
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
ensure_binary(P) when is_pid(P) ->
    ensure_binary(pid_to_list(P));
ensure_binary(B) when is_binary(B) ->
    B.

get_stats(Node, Params) ->
    Date = case [lists:keyfind(K, 1, Params) || K<-["year","month","day"]] of
        [false, false, false] -> date();
        L -> list_to_tuple(lists:map(fun list_to_integer/1, L))
    end,
    [
        begin
            Segments = read_day({node, Node}, M, Date),
            PointsPerSegment = 20 div length(Segments),
            {M, [reduce_stats(PointsPerSegment, Stats) || Stats <- Segments]}
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
split_by(_, _) ->
    [].

avg_quad(L) ->
    Total = length(L),
    round(math:sqrt(lists:foldl(
        fun(I, Acc) ->
            Acc + I*I
        end,
        0,
        L
    ) / (Total) * (Total - 1))).
