-module(enodeman_remote_module).

-export([
    get_processes_info/1,
    build_trees/1
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

%TODO: it uses a lot of memory on remote machine
% as it doesn't use :qa
build_trees(SkipApps) ->
    Apps = [A || {A, _, _} <- application:which_applications(),
        not lists:member(A, SkipApps) ],
    [{A, build_app_tree(A)} || A <- Apps].

build_app_tree(App) ->
    case application_controller:get_master(App) of
        undefined -> [];
        PidMaster ->
            {PidSup, _Name} = application_master:get_child(PidMaster),
            [{PidMaster, fold_tree2(PidSup)}]
    end.

fold_tree2(SupPid) -> 
    ChildSpecs = supervisor:which_children(SupPid),
    [{SupPid, 
        lists:foldl(
            fun ({_N, P, worker, _}, Childs) -> [P | Childs];
                ({_N, P, supervisor, _}, Childs) -> [fold_tree2(P) | Childs]
            end, [], ChildSpecs)}
    ].
