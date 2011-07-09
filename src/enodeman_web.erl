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
    Req:serve_file("index.html", "priv");
handle_request(Req, "/js") ->
    Req:serve_file("enodeman.js", "priv");
handle_request(Req, "/favicon.ico") ->
    Req:ok({"image/x-icon", ""});
handle_request(Req, Path) ->
    handle_request(Req, Path, Req:parse_qs()).

handle_request(Req, "/" ++ Path, Params) ->
    try
        Result = case re:split(Path, "\/", [{return, list}]) of
            [Node] ->
                enodeman_api:connect(Node, Params);
            [Node, Action] ->
                Fun = list_to_atom(Action),
                Pid = enodeman_nodes:node_to_pid(Node),
                enodeman_api:Fun(Pid, Params)
        end,
        Req:ok({"application/json", mochijson2:encode(Result)})
    catch 
        _:{json_encode, _} -> Req:ok("oops, wrong JSON")
    end.
