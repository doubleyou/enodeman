-module(enodeman_util).
-export([
    get_env/1,
    warn/2,
    warn/3,
    err/2,
    err/3,
    info/2,
    info/3,
    now/0
]).

get_env(Key) ->
    {ok, V} = application:get_env(enodeman, Key),
    V.

warn(Module, Format) -> warn(Module, Format, []).
warn(Module, Format, Args) -> 
    logit(warning_msg, Module, Format, Args).

err(Module, Format) -> err(Module, Format, []).
err(Module, Format, Args) ->
    logit(error_msg, Module, Format, Args).


info(Module, Format) -> info(Module, Format, []).
info(Module, Format, Args) ->
    logit(info_msg, Module, Format, Args).

logit(Func, M,F,A) ->
    Format = "[~p] " ++ F,
    Args = [M | A],
    try
        apply(error_logger, Func, [Format, Args])
    catch _:_ ->
        err(?MODULE, "error while running ~p:~p~nArgs:~p~n", [?MODULE, Func, {M,F,A}])
    end.

now() ->
    {MegaSec, Sec, MicroSec} = erlang:now(),
    (MegaSec * 1000000 + Sec) * 1000 + MicroSec div 1000.
