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
            {"/ddfs",  ddfs_web, []}%,
            %{"/proxy",  disco_web, []}, % TODO add a module for proxy
            %{"/", disco_web, []} %TODO add handler for other paths
        ]}
    ]),
    %{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],[{env, [{dispatch, Dispatch}]}]).

    {ok, Pid} = cowboy:start_http(http, 100, [{port, Port}], [
            {env, [{dispatch, Dispatch}]}
        ]),
    lager:info("web server (cowboy) starts with ~p", [Dispatch]),
    {ok, Pid}.

