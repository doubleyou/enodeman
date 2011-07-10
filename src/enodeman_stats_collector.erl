-module(enodeman_stats_collector).
-behaviour(gen_server).
-export([
    start_link/0
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

init(_) ->
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({new_source, {Type, Name, StartTime, Interval}, Stats}, State) ->
    {noreply, [{{Type, Name}, {StartTime, Interval, Stats}} | State]};
handle_cast({update, Id, Update}, State) ->
    {StartTime, Interval, OldStats} = proplists:get_value(Id, State),
    NewStats = update_stats(Update, OldStats),
    {
        noreply,
        lists:keyreplace(Id, 1, State, {Id, {StartTime, Interval, NewStats}})
    };
handle_cast({stop, Id = {Type, Name}}, State) ->
    {StartTime, Interval, Stats} = proplists:get_value(Id, State),
    [
        begin
            O = riakc_obj:new(
                riak_bucket(Type, Name, Metric),
                riak_key(),
                term_to_binary({StartTime, Interval, lists:reverse(Data)})
            ),
            simple_riak_client:do(put, [O, [{w, 2}, {dw, 1}], 1000])
        end
        || {Metric, Data} <- Stats
    ],
    {noreply, lists:keydelete(Id, 1, State)};
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
ensure_binary(P) when is_pid(P) ->
    ensure_binary(pid_to_list(P));
ensure_binary(B) when is_binary(B) ->
    B.
