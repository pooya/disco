
-module(ddfs_put).

-include_lib("kernel/include/file.hrl").

-include("common_types.hrl").
-include("ddfs.hrl").

-export([start/1]).

% maximum file size: 1T
-define(MAX_RECV_BODY, (1024*1024*1024*1024)).

-spec start(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start(Port) ->
    ddfs_util:start_web(Port, fun(Req) -> loop(Req:get(path), Req) end, ?MODULE).

-spec loop(path(), term()) -> _.
loop("/proxy/" ++ Path, Req) ->
    {_Node, Rest} = mochiweb_util:path_split(Path),
    {_Method, RealPath} = mochiweb_util:path_split(Rest),
    loop([$/|RealPath], Req);
loop("/ddfs/" ++ BlobName, Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    case {Method, valid_blob(catch ddfs_util:unpack_objname(BlobName))} of
        {<<"PUT">>, true} ->
            try case ddfs_node:put_blob(BlobName) of
                    {ok, Path, Url} ->
                        receive_blob(Req1, {Path, BlobName}, Url);
                    {error, Path, Error} ->
                        error_reply(Req1, "Could not create path for blob",
                                    Path, Error);
                    {error, no_volumes} ->
                        cowboy_req:reply(500, [], <<"No volumes">>, Req1);
                    full ->
                        cowboy_req:reply(503, [], <<"Maximum number of uploaders reached. ",
                                      "Try again later">>);
                    {error, Error} ->
                        error_reply(Req1, "Could not put blob", BlobName, Error)
                    end
            catch K:V ->
                    error_logger:info_msg("~p: error putting ~p on ~p: ~p:~p",
                                          [?MODULE, BlobName, node(), K, V]),
                    error_reply(Req1, "Could not put blob onto", BlobName, {K,V})
            end;
        {<<"PUT">>, _} ->
            cowboy_req:reply(403, [], <<"Invalid blob name">>, Req1);
        _ ->
            cowboy_req:reply(501, [], <<"Method not supported">>, Req1)
    end;
loop(_, Req) ->
    cowboy_req:reply(404, Req).

-spec valid_blob({'EXIT' | binary(),_}) -> boolean().
valid_blob({'EXIT', _}) -> false;
valid_blob({Name, _}) ->
    ddfs_util:is_valid_name(binary_to_list(Name)).

-spec receive_blob(term(), {path(), path()}, url()) -> _.
receive_blob(Req, {Path, Fname}, Url) ->
    disco_profile:timed_run(
        fun() ->
            Dir = filename:join(Path, Fname),
            case prim_file:read_file_info(Dir) of
                {error, enoent} ->
                    Tstamp = ddfs_util:timestamp(),
                    Partial = lists:flatten(["!partial-", Tstamp, ".", Fname]),
                    Dst = filename:join(Path, Partial),
                    case prim_file:open(Dst, [write, raw, binary]) of
                        {ok, IO} ->
                            receive_blob(Req, IO, Dst, Url);
                        Error -> error_reply(Req, "Opening file failed", Dst, Error)
                    end;
                _ ->
                    error_reply(Req, "File exists", Dir, Dir)
            end
        end, ?MODULE).

-spec receive_blob(term(), file:io_device(), file:filename(), url()) -> _.
receive_blob(Req, IO, Dst, Url) ->
    {ok, BodySize, Req1} = cowboy_req:parse_header(<<"content-length">>, Req),
    {Path, Req2} = cowboy_req:path(Req1),
    error_logger:info_msg("PUT BLOB: ~p (~p bytes) on ~p", [Path, BodySize, node()]),
    case receive_body(Req2, IO) of
        ok ->
            [_, Fname] = string:tokens(filename:basename(Dst), "."),
            Dir = filename:join(filename:dirname(Dst), Fname),
            % NB: Renaming is not atomic below, thus there's a small
            % race condition if two clients are PUTting the same blob
            % concurrently and finish at the same time. In any case the
            % file should not be corrupted.
            case ddfs_util:safe_rename(Dst, Dir) of
                ok ->
                    cowboy_req:respon(201, [{<<"content-type">>, <<"application/json">>}],
                                      ["\"", Url, "\""], Req2);
                {error, {rename_failed, E}} ->
                    error_reply(Req2, "Rename failed", Dst, E);
                {error, {chmod_failed, E}} ->
                    error_reply(Req2, "Mode change failed", Dst, E);
                {error, file_exists} ->
                    error_reply(Req2, "File exists", Dst, Dir)
            end;
        Error ->
            error_reply(Req2, "Write failed", Dst, Error)
    end.

-spec receive_body(term(), file:io_device()) -> _.
receive_body(Req, IO) ->
    {Req1, Len} = (catch receive_body_into(Req, IO, 0)),
    case Len of
        % R == <<>> or undefined if body is empty
        R when is_integer(R); R =:= <<>>; R =:= undefined ->
            {Path, _Req1} = cowboy_req:path(Req1), % TODO
            error_logger:info_msg("PUT BLOB done with ~p (~p) on ~p",
                                  [Path, R, node()]),
            case [file:sync(IO), file:close(IO)] of
                [ok, ok] -> ok;
                E -> hd([X || X <- E, X =/= ok])
            end;
        Error ->
            {Path, _Req1} = cowboy_req:path(Req1), % TODO
            error_logger:info_msg("PUT BLOB error for ~p on ~p: ~p",
                                  [Path, node(), Error]),
            Error
    end.

-spec receive_body_into(term(), file:io_device(), non_neg_integer()) -> _.
receive_body_into(Req, IO, Size) ->
    case Size > ?MAX_RECV_BODY of
        true -> throw(io_lib:format("Size is ~p and is too large.", [Size]));
        false ->
            case cowboy_req:body(Req) of
                {ok, Data, Req1} ->
                    ok = file:write(IO, Data),
                    {Req1, Size + byte_size(Data)};
                {more, Data, Req1} ->
                    ok = file:write(IO, Data),
                    receive_body_into(Req1, IO, Size + byte_size(Data))
            end
    end.


-spec error_reply(term(), nonempty_string(), path(), term()) -> _.
error_reply(Req, Msg, Dst, Err) ->
    M = io_lib:format("~s (path: ~s): ~p", [Msg, Dst, Err]),
    error_logger:warning_msg("Error response for ~p on ~p: ~p (error ~p)",
                             [Dst, node(), Msg, Err]),
    cowboy_req:reply(500, [], M, Req).
