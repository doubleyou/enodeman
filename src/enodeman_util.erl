-module(enodeman_util).
-export([
    get_env/1
]).

get_env(Key) ->
    {ok, V} = application:get_env(enodeman, Key),
    V.
