-module(enodeman_remote_module).

-export([
    get_processes_info/1
]).

get_processes_info(Types) ->
    [ 
        { P, 
            [
                case process_info(P, T) of
                    undefined -> {T, <<"">>};
                    [] -> {T, <<"">>};
                    V -> V
                end ||  T <- Types]
        } || P <- processes() 
    ].
