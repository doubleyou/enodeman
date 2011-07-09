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

handle_request(Req, "/" ++ Action, Params) ->
    %% TODO: write this in a more sophisticated way later, atom leaks
    Fun = list_to_atom(Action),
    try
        Result = enodeman_api:Fun(Req, Params),
        Req:ok({"application/json", mochijson2:encode(Result)})
    catch
        %% TODO: for debugging purposes only
        _:{json_encode, _} -> Req:ok("oops, wrong JSON");
        _:_ -> Req:not_found()
    end.
