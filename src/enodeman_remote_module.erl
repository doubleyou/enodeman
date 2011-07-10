-module(enodeman_remote_module).

-export([
    get_processes_info/0
]).

get_processes_info() ->
    Types = [registered_name, initial_call, memory, reductions, message_queue_len],
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
