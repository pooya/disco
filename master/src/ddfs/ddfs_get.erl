-module(ddfs_get).
-export([handle/2, init/3, terminate/3]).

-include_lib("kernel/include/file.hrl").

-include("common_types.hrl").
-include("ddfs.hrl").

-export([start/2, serve_ddfs_file/3, serve_disco_file/3]).

-spec start(non_neg_integer(), {path(), path()}) -> {ok, pid()} | {error, term()}.
start(Port, Roots) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[...]", ?MODULE, [Roots]}
        ]}
    ]),
    error_logger:info_msg("Started ~p at ~p on port ~p", [?MODULE, node(), Port]),
    cowboy:start_http(http, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]).

init(_Type, Req, [Roots]) ->
    {ok, Req, Roots}.
terminate(_Reason, _Req, _State) ->
    ok.

handle(Req, Roots) ->
    {Path, Req1} = cowboy_req:path(Req),
    {ok, Req2} =  loop(binary_to_list(Path), Req1, Roots),
    {ok, Req2, Roots}.

-spec serve_ddfs_file(path(), path(), term()) -> _.
serve_ddfs_file(DdfsRoot, Path, Req) ->
    loop(Path, Req, {DdfsRoot, none}).

-spec serve_disco_file(path(), path(), term()) -> _.
serve_disco_file(DiscoRoot, Path, Req) ->
    loop(Path, Req, {none, DiscoRoot}).

-spec loop(path(), term(), {path() | none, path() | none}) -> _.
loop("/proxy/" ++ Path, Req, Roots) ->
    {_Node, Rest} = mochiweb_util:path_split(Path),
    {_Method, RealPath} = mochiweb_util:path_split(Rest),
    loop([$/|RealPath], Req, Roots);

loop("/ddfs/" ++ Path, Req, {DdfsRoot, _DiscoRoot}) ->
    send_file(Req, Path, DdfsRoot);

loop("/disco/" ++ Path, Req, {_DdfsRoot, DiscoRoot}) ->
    send_file(Req, Path, DiscoRoot);

loop(_Path, Req, _Roots) ->
    cowboy_req:reply(404, Req).

allowed_method(<<"GET">>) ->
    true;
allowed_method(<<"HEAD">>) ->
    true;
allowed_method(_) ->
    false.

-spec send_file(term(), path(), path()) -> _.
send_file(Req, Path, Root) ->
    {Method, Req1} = cowboy_req:method(Req),
    case {allowed_method(Method), mochiweb_util:safe_relative_path(Path)} of
        {true, undefined} ->
            cowboy_req:reply(404, Req1);
        {true, SafePath} ->
            try case ddfs_node:gate_get_blob() of
                    ok ->
                        send_file(Req1, filename:join(Root, SafePath));
                    full ->
                        cowboy_req:reply(503, [],
                                     <<"Maximum number of downloaders reached. ",
                                      "Try again later">>, Req1)
                end
            catch K:V ->
                    error_logger:info_msg("~p: error getting ~p on ~p: ~p:~p",
                                          [?MODULE, Path, node(), K, V]),
                    cowboy_req:reply(403, [], <<"Disco node is busy">>, Req1)
            end;
        _ ->
            cowboy_req:reply(501, [], <<"Method not supported">>, Req1)
    end.

-spec send_file(term(), path()) -> _.
send_file(Req, Path) ->
    case prim_file:read_file_info(Path) of
        {ok, #file_info{type = regular, size = Size}} ->
            F = fun(Socket, Transport) ->
                    Transport:sendfile(Socket, Path)
                end,
            Req1 = cowboy_req:set_resp_body_fun(Size, F, Req),
            cowboy_req:reply(200, [{<<"content-type">>, <<"application/octet-stream">>}], Req1);
        {ok, _} ->
            cowboy_req:reply(403, [], <<"Forbidden">>, Req);
        {error, enoent} ->
            cowboy_req:reply(404, Req);
        _ ->
            cowboy_req:reply(500, [], <<"Access failed">>, Req)
    end.
