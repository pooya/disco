-module(web_server).

-export([start/1]).

-include("config.hrl").

-spec start(string()) -> {ok, pid()} | {error, term()}.
start(Port) ->
    disco_profile:new_histogram(?MODULE),
    disco_profile:new_histogram(disco_web),
    disco_profile:new_histogram(ddfs_web),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/disco", disco_web, []},
            {"/ddfs",  ddfs_web, []},
            {"/proxy",  disco_web, []}, % TODO add a module for proxy
            {"/", disco_web, []} %TODO add handler for other paths
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
            env, {env, [{dispatch, Dispatch}]}
        ]).

loop("/proxy/" ++ Path, Req) ->
    % Unwrap '/proxy/<nodename>/<meth>/path' to '/path'.
    {_Node, Rest} = mochiweb_util:path_split(Path),
    {_Meth, RealPath} = mochiweb_util:path_split(Rest),
    loop("/" ++ RealPath, Req);
loop("/" ++ Path = P, Req) ->
    disco_profile:timed_run(
        fun() ->
            {Root, _Rest} = mochiweb_util:path_split(Path),
            case lists:member(Root, ?HANDLERS) of
                true ->
                    Module = case Root of
                        "ddfs" -> ddfs_web;
                        "disco" -> disco_web
                    end,
                    disco_profile:timed_run(
                        fun() ->
                                dispatch(Req, Module, P)
                        end, Module);
                false when Path =:= "" ->
                    Req:serve_file("index.html", docroot());
                _ ->
                    Req:serve_file(Path, docroot())
            end
        end, ?MODULE);
loop(_, Req) ->
    Req:not_found().

docroot() ->
    disco:get_setting("DISCO_WWW_ROOT").

dispatch(Req, Module, Path) ->
    erlang:put(mochiweb_request_force_close, true),
    try Module:op(Req:get(method), Path, Req)
    catch
        E ->
            lager:error("Request ~p failed: ~p: ~p",
                        [Req:get(path), E, erlang:get_stacktrace()]),
            Req:respond({400, [], [disco:format("~p", [E])]});
        K:V ->
            lager:error("Request ~p failed (~p:~p): ~p",
                        [Req:get(path), K, V, erlang:get_stacktrace()]),
            Req:respond({500, [], ["Internal server error"]})
    end.
