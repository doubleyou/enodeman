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
    Req:ok({"text/html", "ok"}).
