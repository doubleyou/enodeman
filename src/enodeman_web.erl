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
        Req:ok({"application/javascript", mochijson2:encode(Result)})
    catch 
        _:{json_encode, _} -> Req:ok("oops, wrong JSON")
    end.

%XXX: remove debug code
process_path_request(["nodes"],_) ->
    [
        {<<"page">>,1},
        {<<"total">>,1},
        {<<"records">>,2},
        {<<"rows">>,[
            {struct, [
                {<<"id">>,node1},
                {<<"cell">>, [node1, <<"0.00">>, <<"1000.00">>, null]}
            ]},
            {struct, [
                {<<"id">>,node2},
                {<<"cell">>, [node2, <<"0.00">>, <<"1000.00">>, null]}
            ]}]}
    ];
%XXX: remove debug code
process_path_request([_, "processes"],_) ->
    [
        {<<"page">>,1},
        {<<"total">>,1},
        {<<"records">>,2},
        {<<"rows">>,[
            {struct, [
                {<<"id">>,process_name_1},
                {<<"cell">>, [process_name_1, <<"11231231">>, <<"13">>, null]}
            ]},
            {struct, [
                {<<"id">>,process_name_2},
                {<<"cell">>, [process_name_2, <<"123123123">>, <<"14">>, null]}
            ]}]}
    ];
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
