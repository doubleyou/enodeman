-module(enodeman_web).
-export([
    start/0
]).

start() ->
    mochiweb_http:start([
            {ip, "0.0.0.0"},
            {port, enodeman_util:get_env(web_port)},
            {loop, fun handle_request/1}
        ]).

handle_request(Req) ->
    handle_request(Req, Req:get(path)).

handle_request(Req, "/") ->
    handle_request(Req, "/www/index.html");
handle_request(Req, "/www/" ++ File) ->
    Req:serve_file(File, "priv/www");
handle_request(Req, "/favicon.ico") ->
    Req:ok({"image/x-icon", ""});
handle_request(Req, Path) ->
    handle_request(Req, Path, Req:parse_qs()).

handle_request(Req, "/" ++ Path, Params) ->
    try
        Words = re:split(Path, "\/", [{return, list}]),
        Result = process_path_request(Words, Params),
        %enodeman_util:info(?MODULE, "handle_request ~p (~p)~nResult:~n~p~n", [Path, Params, Result]),

        Encoded = mochijson2:encode(Result),
        MaybeJSONP = case proplists:get_value("callback", Params) of
            undefined -> Encoded;
            V -> V ++ "(" ++ Encoded ++ ");"
        end,
        Req:ok({"application/javascript", MaybeJSONP})
    catch 
        _:{json_encode, _} -> Req:ok({"text/html", "oops, wrong JSON"})
    end.

process_path_request(["nodes"],Params) ->
    Nodes = enodeman_nodes:which_nodes(),
    enodeman_api:connect(node(), Params),
    [
        {<<"page">>,1},
        {<<"total">>,1},
        {<<"records">>,1},
        {<<"rows">>,[
            {struct, [
                {<<"id">>, list_to_binary(Node)},
                {<<"cell">>, [list_to_binary(Node), <<"0.00">>, <<"1000.00">>]}
            ]}
            || Node <- Nodes
        ]}
    ];
process_path_request(["node_metrics"],_) ->
    enodeman_api:node_metrics();
process_path_request(["proc_metrics"],_) ->
    enodeman_api:proc_metrics();
process_path_request([Node], Params) ->
    enodeman_api:connect(Node, Params);
% hack
process_path_request([_, "processes_tree" = Action], Params) ->
    Node = "enodeman@127.0.0.1",
    enodeman_api:connect(Node, Params), %TODO: remove it?
    Fun = list_to_atom(Action),
    Pid = enodeman_nodes:node_to_pid(Node),
    enodeman_api:Fun(Pid, Params);
process_path_request([Node, Action], Params) ->
    enodeman_api:connect(Node, Params), %TODO: remove it?
    Fun = list_to_atom(Action),
    Pid = enodeman_nodes:node_to_pid(Node),
    enodeman_api:Fun(Pid, Params).
