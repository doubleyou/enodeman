-module(enodeman_remote_module).

-export([
    get_processes_info/1,
    build_tree/0
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

skip_apps() -> enodeman_util:get_env(not_monitored_apps).

build_tree() ->
    Apps = get_apps(),
    [{A, build_app_tree(A)} || A <- Apps].

build_app_tree(App) ->
    PidMaster = application_controller:get_master(App),
    {PidSup, _Name} = application_master:get_child(PidMaster),
    fold_tree([PidSup], [{PidMaster, [PidSup]}]).

% 1 argument. Pid of Supervisor
% 2 argument. Acc:  ([{PidSup, [PidChilds]}])
fold_tree([], Acc) -> Acc;
fold_tree([SupPid | Rest], Acc) -> 
    ChildSpecs = supervisor:which_children(SupPid),
    {CurChilds,NewRest} = lists:foldl(
        fun ({_N, Pid, worker, _}, {Childs, Sups}) -> 
                {[Pid | Childs], Sups};
            ({_N, Pid, supervisor, _}, {Childs, Sups}) -> 
                {[Pid | Childs], [Pid | Sups]}
        end, {[], Rest}, ChildSpecs),
    fold_tree(NewRest, [{SupPid, CurChilds} | Acc]).

get_apps() ->
    [A 
        || {A, _, _} <- application:which_applications(),
        not lists:member(A, skip_apps())
    ].
