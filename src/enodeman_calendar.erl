-module(enodeman_calendar).
-export([
    days_between/2
]).

%% TODO: use PropEr here for tests!!!

days({Y1, all, all}, {Y2, all, all}) when Y1 > Y2 ->
    [];
days({Y1, all, all}, {Y2, all, all}) ->
    lists:foldl(
        fun(Y, Acc) ->
            Acc ++ days({Y, 1, all}, {Y, 12, all})
        end,
        [],
        lists:seq(Y1, Y2)
    );
days({Y, M1, all}, {Y, M2, all}) when M1 > M2 ->
    [];
days({Y, M1, all}, {Y, M2, all}) ->
    lists:foldl(
        fun (M, Acc) ->
            Acc ++ [{Y, M, D}
                || D <- lists:seq(1, calendar:last_day_of_the_month(Y, M))]
        end,
        [],
        lists:seq(M1, M2)
    );
days({Y, M, D1}, {Y, M, last}) ->
    days({Y, M, D1}, {Y, M, calendar:last_day_of_the_month(Y, M)});
days({Y, M, D1}, {Y, M, D2}) ->
    [{Y, M, D} || D <- lists:seq(D1, D2)];
days({Y, M1, D1}, {Y, M2, D2}) ->
    days({Y, M1, D1}, {Y, M1, last}) ++
    days({Y, M1 + 1, all}, {Y, M2 - 1, all}) ++
    days({Y, M2, 1}, {Y, M2, D2});
days({Y1, M1, D1}, {Y2, M2, D2}) ->
    days({Y1, M1, D1}, {Y1, 12, last}) ++
    days({Y1 + 1, all, all}, {Y2 - 1, all, all}) ++
    days({Y2, 1, 1}, {Y2, M2, D2}).
