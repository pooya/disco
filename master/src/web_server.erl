-module(web_server).

-export([start/1]).

-include("config.hrl").

-spec start(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start(Port) ->
    disco_profile:new_histogram(?MODULE),
    disco_profile:new_histogram(disco_web),
    disco_profile:new_histogram(ddfs_web),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/disco/[...]", disco_web, []},
            {"/ddfs/[...]", ddfs_web, []},
            {"/[...]", disco_file_server, []}
            %{"/proxy",  disco_web, []}, % TODO add a module for proxy
        ]}
    ]),

    {ok, Pid} = cowboy:start_http(http, 100, [{port, Port}], [
            {env, [{dispatch, Dispatch}]}
        ]),
    lager:info("web server (cowboy) starts."),
    {ok, Pid}.

