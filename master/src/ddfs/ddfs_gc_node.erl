-module(ddfs_gc_node).
<<<<<<< HEAD
-export([start_gc_node/4]).
=======
<<<<<<< HEAD
-export([gc_node/2]).
=======
-export([start_gc_node/4,
         start_s3_gc_node/3]).
>>>>>>> fcd832e... uploads to s3 for s3 urls returns by master
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.

-include_lib("kernel/include/file.hrl").

-include("common_types.hrl").
-include("config.hrl").
-include("ddfs.hrl").
-include("ddfs_gc.hrl").

% The module contains the node-local portion of the DDFS GC/RR
% algorithm.

<<<<<<< HEAD
=======
<<<<<<< HEAD
-spec gc_node(pid(), disco_util:timestamp()) -> 'orphans_done'.
gc_node(Master, Now) ->
=======
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
-spec start_gc_node(node(), pid(), erlang:timestamp(), phase()) -> pid().
start_gc_node(Node, Master, Now, Phase) ->
    spawn_link(Node, fun () -> gc_node_init(Master, Now, Phase) end).

<<<<<<< HEAD
-spec gc_node_init(pid(), erlang:timestamp(), phase()) -> 'ok'.
gc_node_init(Master, Now, Phase) ->
    register(?MODULE, self()),
    % All phases of GC/RR require that we build a snapshot of our
    % node-local DDFS content across all volumes.
=======
-spec start_s3_gc_node(pid(), erlang:timestamp(), phase()) -> pid().
start_s3_gc_node(Master, Now, Phase) ->
    spawn_link(fun () -> s3_gc_node_init(Master, Now, Phase) end).

-spec gc_node_init(pid(), erlang:timestamp(), phase()) -> 'ok'.
gc_node_init(Master, Now, Phase) ->
    % All phases of GC/RR require that we build a snapshot of our
    % node-local DDFS content across all volumes.
>>>>>>> fcd832e... uploads to s3 for s3 urls returns by master
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
    process_flag(priority, low),
    {Vols, Root} = ddfs_node:get_vols(),
    {_, VolNames} = lists:unzip(Vols),

    % Traverse the volumes and build up a cache of all stored objects
    % (tags and blobs).
    % obj : {Key    :: object_name(),
    %        Vol    :: volume_name(),
    %        Size   :: non_neg_integer(),
    %        in_use :: 'false' | 'true'}
    _ = ets:new(tag, [named_table, set, private]),
    _ = ets:new(blob, [named_table, set, private]),
<<<<<<< HEAD
    traverse(Now, Root, VolNames, blob),
    traverse(Now, Root, VolNames, tag),
    error_logger:info_msg("GC: found ~p blob, ~p tag candidates on ~p",
                          [ets:info(blob, size), ets:info(tag, size), node()]),
    % Now, dispatch to the phase that is running on the master.
    gc_node(Master, Now, Root, Phase).

-spec gc_node(pid(), erlang:timestamp(), path(), phase()) -> 'ok'.
gc_node(Master, Now, Root, Phase)
  when Phase =:= start; Phase =:= build_map; Phase =:= map_wait ->
    check_server(Master, Root),
    local_gc(Master, Now, Root),
    replica_server(Master, Root);

=======
<<<<<<< HEAD
    {Vols, Root} = gen_server:call(ddfs_node, get_vols),
    {_, VolNames} = lists:unzip(Vols),
    traverse(Now, Root, VolNames, "blob", blob),
    traverse(Now, Root, VolNames, "tag", tag),
    error_logger:info_report({"GC: # blobs", ets:info(blob, size)}),
    error_logger:info_report({"GC: # tags", ets:info(tag, size)}),
    node_server(Root),
    delete_orphaned(Master, Now, Root, "blob", blob, ?ORPHANED_BLOB_EXPIRES),
    delete_orphaned(Master, Now, Root, "tag", tag, ?ORPHANED_TAG_EXPIRES),
    Master ! orphans_done.

-spec node_server(nonempty_string()) -> 'ok'.
node_server(Root) ->
    receive
        {{touch, Ets}, M, Obj} ->
            M ! {Obj, take_key(Obj, Ets)},
            node_server(Root);
        {put_blob, M, Obj, DstUrl} ->
            M ! {Obj, send_blob(Obj, DstUrl, Root)},
            node_server(Root);
        done ->
            ok;
        E ->
            ddfs_gc:abort({"GC: Erroneous message received", E},
                requests_failed)
    end.

-spec take_key(binary(), atom()) -> bool().
take_key(Key, Ets) ->
    ets:update_element(Ets, Key, {3, true}).

-spec send_blob(binary(), nonempty_string(), nonempty_string()) ->
    'ok' | {'error', 'crashed' | 'timeout'}.
send_blob(Obj, DstUrl, Root) ->
    [{_, VolName, _}] = ets:lookup(blob, Obj),
    {ok, Path, _} = ddfs_util:hashdir(Obj, "nonode!", "blob", Root, VolName),
    ddfs_http:http_put(filename:join(Path, binary_to_list(Obj)),
        DstUrl, ?GC_PUT_TIMEOUT).

-spec traverse(disco_util:timestamp(), nonempty_string(),
               [nonempty_string()], nonempty_string(), 'blob' | 'tag') -> _.
traverse(Now, Root, VolNames, Mode, Ets) ->
    lists:foldl(
        fun(VolName, _) ->
            ddfs_util:fold_files(filename:join([Root, VolName, Mode]),
                                 fun(Obj, Dir, _) ->
                                     handle_file(Obj, Dir, VolName, Ets, Now)
                                 end, nil)
        end, nil, VolNames).

%%%
%%% O1) Remove leftover !partial. files
%%%
-spec handle_file(nonempty_string(), nonempty_string(),
    nonempty_string(), 'blob' | 'tag', disco_util:timestamp()) -> _.
=======
    traverse(Now, Root, VolNames, blob),
    traverse(Now, Root, VolNames, tag),
    error_logger:info_msg("GC: found ~p blob, ~p tag candidates on ~p",
                          [ets:info(blob, size), ets:info(tag, size), node()]),
    % Now, dispatch to the phase that is running on the master.
    gc_node(Master, Now, Root, Phase).

-spec s3_gc_node_init(pid(), erlang:timestamp(), phase()) -> 'ok'.
s3_gc_node_init(Master, Now, Phase) ->
    put(s3_bucket, disco:get_setting("DISCO_S3_BUCKET")),
    process_flag(priority, low),
    _ = ets:new(tag, [named_table, set, private]),
    _ = ets:new(blob, [named_table, set, private]),
    s3_traverse(Now, "", blob),
    s3_traverse(Now, "", tag),
    error_logger:info_msg("GC: found ~p blob, ~p tag candidates on ~p",
                          [ets:info(blob, size), ets:info(tag, size), node()]),
    % Now, dispatch to the phase that is running on the master.
    s3_gc_node(Master, Now, "", Phase).

-spec gc_node(pid(), erlang:timestamp(), string(), phase()) -> 'ok'.
gc_node(Master, Now, Root, Phase)
  when Phase =:= start; Phase =:= build_map; Phase =:= map_wait ->
    check_server(Master, Root),
    local_gc(Master, Now, Root),
    replica_server(Master, Root);

>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
gc_node(Master, Now, Root, gc) ->
    local_gc(Master, Now, Root),
    replica_server(Master, Root);

gc_node(Master, _Now, Root, Phase)
  when Phase =:= rr_blobs; Phase =:= rr_blobs_wait; Phase =:= rr_tags ->
    replica_server(Master, Root).

<<<<<<< HEAD
=======
-spec s3_gc_node(pid(), erlang:timestamp(), string(), phase()) -> 'ok'.
s3_gc_node(Master, Now, Root, _Phase) ->
    s3_gc(Master, Now, Root),
    replica_server(Master, Root).

>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
%%
%% Node-local object table construction. (build_map / map_wait / gc)
%%

<<<<<<< HEAD
-spec traverse(erlang:timestamp(), path(), [volume_name()], object_type()) -> 'ok'.
=======
-spec traverse(erlang:timestamp(), string(), [volume_name()], object_type()) -> 'ok'.
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
traverse(Now, Root, VolNames, Type) ->
    Mode = case Type of tag -> "tag"; blob -> "blob" end,
    lists:foreach(
      fun(VolName) ->
              DDFSDir = filename:join([Root, VolName, Mode]),
              Handler = fun(Obj, Dir, _Ok) ->
                                handle_file(Obj, Dir, VolName, Type, Now)
                        end,
              ddfs_util:fold_files(DDFSDir, Handler, ok)
      end, VolNames).

<<<<<<< HEAD
-spec handle_file(path(), path(), volume_name(), object_type(), erlang:timestamp())
                 -> 'ok'.
=======
-spec s3_traverse(erlang:timestamp(), string(), object_type()) -> 'ok'.
s3_traverse(_Now, _Root, Type) ->
    Mode = case Type of tag -> "tag"; blob -> "blob" end,
    Bucket = get(s3_bucket),
    {contents, L} = lists:keyfind(contents, 1, disco_aws:list_objects(Bucket)),
    lists:foreach(fun(F) ->
                          {key, Key} = lists:keyfind(key, 1, F),
                          case string:str(Key, Mode) > 0 of
                              false ->
                                  nothing;
                              true ->
                                  ets:insert(Type, {list_to_binary(Key), "", 0, false})
                          end
                  end, L).

-spec handle_file(string(), string(), volume_name(), object_type(), erlang:timestamp())
                 -> 'ok'.
>>>>>>> fcd832e... uploads to s3 for s3 urls returns by master
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
handle_file("!trash" ++ _, _, _, _, _) ->
    ok;
% GC1) Remove leftover !partial. files
handle_file("!partial" ++ _ = File, Dir, _, _, Now) ->
    [_, Obj] = string:tokens(File, "."),
    {_, Time} = ddfs_util:unpack_objname(Obj),
    Diff = timer:now_diff(Now, Time) / 1000,
    Paranoid = disco:has_setting("DDFS_PARANOID_DELETE"),
<<<<<<< HEAD
    Path = filename:join(Dir, File),
    delete_if_expired(unknown, Path, Diff, ?PARTIAL_EXPIRES, Paranoid);
=======
<<<<<<< HEAD
    delete_if_expired(filename:join(Dir, File), Diff, ?PARTIAL_EXPIRES, Paranoid);
handle_file(Obj, _, VolName, Ets, _) ->
    % We could check a checksum of Obj here
    ets:insert(Ets, {list_to_binary(Obj), VolName, false}).

%%%
%%% O2) Remove orphaned tags
%%% O3) Remove orphaned blobs
%%%
-spec delete_orphaned(pid(), disco_util:timestamp(), nonempty_string(),
                      nonempty_string(), 'blob'|'tag', non_neg_integer()) -> _.
delete_orphaned(Master, Now, Root, Mode, Ets, Expires) ->
    Paranoid = disco:has_setting("DDFS_PARANOID_DELETE"),
    lists:foreach(
        fun([Obj, VolName]) ->
            {_, Time} = ddfs_util:unpack_objname(Obj),
            {ok, Path, _} =
                ddfs_util:hashdir(Obj, "nonode!", Mode, Root, VolName),
            FullPath = filename:join(Path, binary_to_list(Obj)),
            Diff = timer:now_diff(Now, Time) / 1000,
            is_really_orphan(Master, Obj) andalso
                delete_if_expired(FullPath, Diff, Expires, Paranoid)
        end, ets:match(Ets, {'$1', '$2', false})).

-spec is_really_orphan(pid(), binary()) -> bool().
is_really_orphan(Master, Obj) ->
    Master ! {is_orphan, self(), Obj},
=======
    delete_if_expired(local, Dir, File, Diff, ?PARTIAL_EXPIRES, Paranoid);
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
handle_file(Obj, Dir, VolName, Type, _) ->
    Size = case prim_file:read_file_info(filename:join(Dir, Obj)) of
               {ok, #file_info{size = S}} -> S;
               _E -> 0
           end,
    ets:insert(Type, {list_to_binary(Obj), VolName, Size, false}).

%%
%% Serve check requests from master (build_map / map_wait)
%%

check_server(Master, Root) ->
<<<<<<< HEAD
=======
>>>>>>> fcd832e... uploads to s3 for s3 urls returns by master
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
    receive
        {check_blob, ObjName} ->
            LocalObj = {ObjName, node()},
            Master ! {check_blob_result, LocalObj, check_blob(ObjName)},
            check_server(Master, Root);
        start_gc ->
            ok;
        E ->
            ddfs_gc:abort({"GC: Erroneous message received", E},
                          node_request_failed)
    end.

-spec check_blob(object_name()) -> check_blob_result().
check_blob(ObjName) ->
    case ets:update_element(blob, ObjName, {4, true}) of
        false -> false;
        true  -> {true, ets:lookup_element(blob, ObjName, 2)}
    end.

<<<<<<< HEAD
% Perform GC
%
% GC2) Remove orphaned tags (old versions and deleted tags)
% GC3) Remove orphaned blobs (blobs not referred by any tag)

local_gc(Master, Now, Root) ->
    delete_orphaned(Master, Now, Root, blob, ?ORPHANED_BLOB_EXPIRES),
    delete_orphaned(Master, Now, Root, tag, ?ORPHANED_TAG_EXPIRES),
    ddfs_gc_main:node_gc_done(Master, gc_run_stats()),
    ddfs_node:rescan_tags(),  % update local node cache
    ok.

-spec obj_stats(object_type()) -> obj_stats().
obj_stats(Type) ->
    ets:foldl(fun({_O, _V, Sz, true}, {Kept, Deleted}) ->
                      {Files, Bytes} = Kept,
                      {{Files + 1, Bytes + Sz}, Deleted};
                 ({_O, _V, Sz, false}, {Kept, Deleted}) ->
                      {Files, Bytes} = Deleted,
                      {Kept, {Files + 1, Bytes + Sz}}
              end, {{0, 0}, {0, 0}}, Type).

-spec gc_run_stats() -> gc_run_stats().
gc_run_stats() ->
    {obj_stats(tag), obj_stats(blob)}.

-spec delete_orphaned(pid(), erlang:timestamp(), path(), object_type(),
                      non_neg_integer()) -> 'ok'.
delete_orphaned(Master, Now, Root, Type, Expires) ->
    Paranoid = disco:has_setting("DDFS_PARANOID_DELETE"),
    lists:foreach(
      fun([Obj, VolName]) ->
              {_, Time} = ddfs_util:unpack_objname(Obj),
              {ok, Path, _} =
                  ddfs_util:hashdir(Obj, "nonode!", atom_to_list(Type), Root, VolName),
              FullPath = filename:join(Path, binary_to_list(Obj)),
              Diff = timer:now_diff(Now, Time) / 1000,
              Test = try ddfs_gc_main:is_orphan(Master, Type, Obj, VolName)
                     catch _:_ -> false
                     end,
              case Test of
                  {ok, Orphan} when Orphan =:= true orelse Orphan =:= unknown ->
                      Deleted = delete_if_expired(Orphan, FullPath, Diff, Expires, Paranoid),
                      true = ets:update_element(Type, Obj, {4, not Deleted});
                  _E ->
                      % Do not delete if not orphan, timeout or error.
                      % Mark the object as in-use for stats.
                      true = ets:update_element(Type, Obj, {4, true})
              end
      end, ets:match(Type, {'$1', '$2', '_', false})).

-spec delete_if_expired(true | unknown, file:filename(), float(),
                        non_neg_integer(), boolean()) -> boolean().
delete_if_expired(Orphan, Path, Diff, Expires, true)
  when Orphan =:= true orelse Diff > Expires ->
    error_logger:info_msg("GC: Deleting expired object (paranoid) at ~p", [Path]),
=======
<<<<<<< HEAD
-spec delete_if_expired(file:filename(), float(),
                        non_neg_integer(), bool()) -> 'ok'.
delete_if_expired(Path, Diff, Expires, true) when Diff > Expires ->
    error_logger:info_report({"GC: Deleting expired object (paranoid)", Path}),
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
    Trash = "!trash." ++ filename:basename(Path),
    Deleted = filename:join(filename:dirname(Path), Trash),
=======
-spec check_blob(object_name()) -> check_blob_result().
check_blob(ObjName) ->
    case ets:update_element(blob, ObjName, {4, true}) of
        false -> false;
        true  -> {true, ets:lookup_element(blob, ObjName, 2)}
    end.

% Perform GC
%
% GC2) Remove orphaned tags (old versions and deleted tags)
% GC3) Remove orphaned blobs (blobs not referred by any tag)

local_gc(Master, Now, Root) ->
    delete_orphaned(local, Master, Now, Root, blob, ?ORPHANED_BLOB_EXPIRES),
    delete_orphaned(local, Master, Now, Root, tag, ?ORPHANED_TAG_EXPIRES),
    ddfs_gc_main:node_gc_done(Master, gc_run_stats()),
    ddfs_node:rescan_tags(),  % update local node cache
    ok.

s3_gc(Master, Now, Root) ->
    delete_orphaned(s3, Master, Now, Root, blob, ?ORPHANED_BLOB_EXPIRES),
    delete_orphaned(s3, Master, Now, Root, tag, ?ORPHANED_TAG_EXPIRES),
    ddfs_gc_main:s3_node_gc_done(Master, gc_run_stats()),
    ok.

-spec obj_stats(object_type()) -> obj_stats().
obj_stats(Type) ->
    ets:foldl(fun({_O, _V, Sz, true}, {Kept, Deleted}) ->
                      {Files, Bytes} = Kept,
                      {{Files + 1, Bytes + Sz}, Deleted};
                 ({_O, _V, Sz, false}, {Kept, Deleted}) ->
                      {Files, Bytes} = Deleted,
                      {Kept, {Files + 1, Bytes + Sz}}
              end, {{0, 0}, {0, 0}}, Type).

-spec gc_run_stats() -> gc_run_stats().
gc_run_stats() ->
    {obj_stats(tag), obj_stats(blob)}.

-spec delete_orphaned(s3 | local, pid(), erlang:timestamp(), string(), object_type(),
                      non_neg_integer()) -> 'ok'.
delete_orphaned(NodeType, Master, Now, Root, Type, Expires) ->
    Paranoid = disco:has_setting("DDFS_PARANOID_DELETE"),
    lists:foreach(
      fun([Obj, VolName]) ->
              {_, Time} = ddfs_util:unpack_objname(Obj),
              {ok, Path, _} =
                  ddfs_util:hashdir(Obj, "nonode!", atom_to_list(Type), Root, VolName),
              Diff = timer:now_diff(Now, Time) / 1000,
              Orphan = try ddfs_gc_main:is_orphan(Master, Type, Obj, VolName)
                       catch _:_ -> false
                       end,
              case Orphan of
                  {ok, true} ->
                      Deleted = delete_if_expired(NodeType, Path, Obj, Diff, Expires, Paranoid),
                      true = ets:update_element(Type, Obj, {4, not Deleted});
                  _E ->
                      % Do not delete if not orphan, timeout or error.
                      % Mark the object as in-use for stats.
                      true = ets:update_element(Type, Obj, {4, true})
              end
      end, ets:match(Type, {'$1', '$2', '_', false})).

-spec delete_if_expired(s3 | local, file:filename(), binary(), float(),
                        non_neg_integer(), boolean()) -> boolean().
delete_if_expired(s3, _Path, Obj, Diff, Expires, true) when Diff > Expires ->
    FullPath = binary_to_list(Obj),
    error_logger:info_msg("S3 GC: Deleting expired object (paranoid, but not) at ~p", [FullPath]),
    Bucket = get(s3_bucket),
    _ = disco_aws:delete(Bucket, FullPath),
    true;
delete_if_expired(local, Path, Obj, Diff, Expires, true) when Diff > Expires ->
    FullPath = filename:join(Path, binary_to_list(Obj)),
    error_logger:info_msg("GC: Deleting expired object (paranoid) at ~p", [FullPath]),
    Trash = "!trash." ++ filename:basename(FullPath),
    Deleted = filename:join(filename:dirname(FullPath), Trash),
>>>>>>> fcd832e... uploads to s3 for s3 urls returns by master
    % Chmod u+w deleted files, so they can removed safely with rm without -f
    _ = prim_file:write_file_info(FullPath, #file_info{mode = 8#00600}),
    _ = prim_file:rename(FullPath, Deleted),
    % Sleep here two prevent master being DDOS'ed by info_reports above
<<<<<<< HEAD
    timer:sleep(100),
    true;
delete_if_expired(true, Path, _Diff, _Expired, false) ->
    % Known garbage is deleted without waiting for timeout expiry.
    error_logger:info_msg("GC: Deleting garbage object at ~p:~p", [node(), Path]),
    _ = prim_file:delete(Path),
    timer:sleep(100),
    true;
delete_if_expired(_Orphan, Path, Diff, Expires, false) when Diff > Expires ->
    error_logger:info_msg("GC: Deleting expired object at ~p:~p", [node(), Path]),
=======
<<<<<<< HEAD
    timer:sleep(100);

delete_if_expired(Path, Diff, Expires, _Paranoid) when Diff > Expires ->
    error_logger:info_report({"GC: Deleting expired object", Path}),
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
    _ = prim_file:delete(Path),
    timer:sleep(100),
    true;

delete_if_expired(_Orphan, Path, _Diff, _Expires, _Paranoid) ->
    error_logger:info_msg("GC: Retaining orphan until expiry at ~p:~p", [node(), Path]),
    false.

%%
%% Re-replication  (rr_blobs / rr_blobs_wait)
%%

<<<<<<< HEAD
-spec replica_server(pid(), path()) -> 'ok'.
=======
=======
    timer:sleep(100),
    true;
delete_if_expired(s3, _Path, Obj, Diff, Expires, _Paranoid) when Diff > Expires ->
    FullPath = binary_to_list(Obj),
    error_logger:info_msg("S3 GC: Deleting expired object at ~p", [FullPath]),
    Bucket = get(s3_bucket),
    _ = disco_aws:delete(Bucket, FullPath),
    timer:sleep(100),
    true;
delete_if_expired(local, Path, Obj, Diff, Expires, _Paranoid) when Diff > Expires ->
    FullPath = filename:join(Path, binary_to_list(Obj)),
    error_logger:info_msg("GC: Deleting expired object at ~p", [FullPath]),
    _ = prim_file:delete(FullPath),
    timer:sleep(100),
    true;

delete_if_expired(_, _Path, _Obj, _Diff, _Expires, _Paranoid) ->
    false.

%%
%% Re-replication  (rr_blobs / rr_blobs_wait)
%%

-spec replica_server(pid(), string()) -> 'ok'.
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
replica_server(Master, Root) ->
    receive
        {put_blob, Replicator, Ref, Blob, PutUrl} ->
            [{_, VolName, _, _}] = ets:lookup(blob, Blob),
            {ok, Path, _} =
                ddfs_util:hashdir(Blob, "nonode!", "blob", Root, VolName),
            SrcPath = filename:join(Path, binary_to_list(Blob)),
            Replicator ! {Ref, Blob, PutUrl, do_put(Blob, SrcPath, PutUrl)},
            replica_server(Master, Root);
        end_rr ->
            ok
    end.

<<<<<<< HEAD
-spec do_put(object_name(), path(), nonempty_string()) ->
=======
-spec do_put(object_name(), string(), nonempty_string()) ->
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
                    {'ok', object_name(), [binary(),...]} | {'error', term()}.
do_put(Blob, SrcPath, PutUrl) ->
    case ddfs_http:http_put(SrcPath, PutUrl, ?GC_PUT_TIMEOUT) of
        {ok, Body} ->
            Resp = try mochijson2:decode(Body)
                   catch _ -> invalid; _:_ -> invalid
                   end,
            case Resp of
                invalid ->
                    {error, {server_error, Body}};
                Resp when is_binary(Resp) ->
                    case ddfs_util:parse_url(Resp) of
                        not_ddfs -> {error, {server_error, Body}};
<<<<<<< HEAD
                        _        -> {ok, Blob, [Resp]}
=======
                        _ ->        {ok, Blob, [Resp]}
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
                    end;
                _Err ->
                    {error, {server_error, Body}}
            end;
        {error, E} ->
            {error, E}
    end.
<<<<<<< HEAD
=======
>>>>>>> fcd832e... uploads to s3 for s3 urls returns by master
>>>>>>> 8c4715a... Moved setup.py to the root directory to make pip happy.
