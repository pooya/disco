-module(hdfs).
-export([save_to_hdfs/4, get_compliant_name/1]).

get_data_node_link(NameNode, HdfsPath, User) ->
    URL = "http://" ++ NameNode ++ "/webhdfs/v1" ++ HdfsPath ++ "?op=CREATE&user.name=" ++ User,
    Response = httpc:request(put, {URL, [], "text/plain", <<"">>}, [], []),
    {ok, {{_,307,_}, L, []}} = Response,
    Loc = lists:keyfind("location", 1, L),
    element(2, Loc).

put_to_data_node(URL, LocalPath) ->
    BodyFun = fun(Fd) ->
        case file:read(Fd, 512) of
        eof ->
            eof;
        {ok, Data} ->
            {ok, Data, Fd}
        end
    end,
    {ok, Fd1} = file:open(LocalPath, [binary, read, raw]),
    Response = httpc:request(put, {URL, [], "text/plain",
            {chunkify, BodyFun, Fd1}}, [], []),
    {ok, {{_,201,_}, _, []}} = Response.

-spec save_to_hdfs(string(), string(), string(), string()) -> ok.
save_to_hdfs(NameNode, HdfsPath, User, LocalPath) ->
    DataNodeUrl = get_data_node_link(NameNode, HdfsPath, User),
    {ok, _} = put_to_data_node(DataNodeUrl, LocalPath),
    ok.

-spec get_compliant_name(string()) -> string().
get_compliant_name(Name) ->
    re:replace(Name, ":", "_", [global, {return, list}]).

% run will be
% inets:start(),
% hdfs:save_to_hdfs("devdisco03:50070", "/user/shayan/test18", "shayan", "test_data.dat").
