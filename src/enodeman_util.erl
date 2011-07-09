-module(enodeman_util).
-export([
    get_env/1,
    warning/1,
    warning/2
]).

get_env(Key) ->
    {ok, V} = application:get_env(enodeman, Key),
    V.

warning(Format) -> warning(Format, []).
warning(Format, Args) -> error_logger:warning_msg(Format, Args).

