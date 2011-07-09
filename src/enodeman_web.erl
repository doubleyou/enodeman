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
%XXX: debug code
handle_request(Req, "/debug_test") ->
    R = mochijson2:encode({struct, [{"aaa", 234}]}),
    Req:ok({"application/json", R});
handle_request(Req, Path) ->
    handle_request(Req, Path, Req:parse_qs()).

handle_request(Req, "/" ++ Path, Params) ->
    try
        Words = re:split(Path, "\/", [{return, list}]),
        Result = process_path_request(Words, Params),
        Req:ok({"application/json", mochijson2:encode(Result)})
    catch 
        _:{json_encode, _} -> Req:ok("oops, wrong JSON")
    end.

process_path_request(["node_metrics"],_) ->
    enodeman_api:node_metrics();
process_path_request(["proc_metrics"],_) ->
    enodeman_api:proc_metrics();
process_path_request([Node], Params) ->
    enodeman_api:connect(Node, Params);
process_path_request([Node, Action], Params) ->
    enodeman_api:connect(Node, Params), %TODO: remove it?
    Fun = list_to_atom(Action),
    Pid = enodeman_nodes:node_to_pid(Node),
    enodeman_api:Fun(Pid, Params).
