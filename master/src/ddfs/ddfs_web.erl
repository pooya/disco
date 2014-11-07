
-module(ddfs_web).

-include("config.hrl").
-include("ddfs.hrl").
-include("ddfs_tag.hrl").

-export([handle/2, init/3, terminate/3]).

-type json() :: binary() | [binary()] | {'struct', [{binary(), json()}]}.

-spec parse_tag_attribute(string(), atom()) ->
    {nonempty_string(), attrib() | 'all' | 'unknown_attribute'}.
parse_tag_attribute(TagAttrib, DefaultAttrib) ->
    case mochiweb_util:path_split(TagAttrib) of
        {T, ""} -> {T, DefaultAttrib};
        {T, "ddfs:urls"} -> {T, urls};
        {T, "ddfs:read-token"} -> {T, read_token};
        {T, "ddfs:write-token"} -> {T, write_token};
        {T, "ddfs:" ++ _} -> {T, unknown_attribute};
        {T, A} -> {T, {user, list_to_binary(A)}}
    end.

init(_Type, Req, []) ->
    {ok, Req, undefined}.
terminate(_Reason, _Req, _State) ->
    ok.

handle(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    {Path, Req2} = cowboy_req:path(Req1),
    lager:info("Path is: ~p", [Path]),
    Req4 = case op(Method, binary_to_list(Path), Req2) of
        {ok, Req3} -> Req3;
        Req3       -> Req3
    end,
    {ok, Req4, State}.

-spec parse_auth_token(term()) -> {token(), term()}.
parse_auth_token(Req) ->
    {AuthHdr, Req1} = cowboy_req:qs_val(<<"authorization">>, Req, undefined),
    Token = case AuthHdr of
        undefined ->
            null;
        <<"Basic ",AuthBin/binary>> ->
            Auth = binary_to_list(AuthBin),
            case base64:decode(Auth) of
                <<"token:", T/binary>> ->
                    T;
                _ ->
                    null
            end;
        _ ->
            % Unknown auth, or basic auth that does not follow the spec.
            null
    end,
    {Token, Req1}.

-spec op(atom(), string(), term()) -> _.
op(<<"POST">>, "/ddfs/ctrl/hosted_tags", Req) ->
    Fun =
        fun(BinHost, _Size) ->
            Host = binary_to_list(BinHost),
            case ddfs_master:get_hosted_tags(Host) of
                {ok, Tags} ->
                    okjson(Tags, Req);
                E ->
                    lager:warning("/ddfs/ctrl/hosted_tags failed for ~p", [Host]),
                    on_error(E, Req)
            end
        end,
    process_payload(Fun, Req);

op(<<"GET">>, "/ddfs/ctrl/gc_stats", Req) ->
    case ddfs_master:gc_stats() of
        {ok, none} ->
            okjson(<<"">>, Req);
        {ok, {{{{TKF, TKB},{TDF, TDB}}, {{BKF, BKB}, {BDF,BDB}}}, TStamp}} ->
            When = disco_util:format_timestamp(TStamp),
            Resp = {struct, [{<<"timestamp">>, When},
                             {<<"stats">>, [{<<"Tags kept">>, [TKF, TKB]},
                                            {<<"Tags deleted">>, [TDF, TDB]},
                                            {<<"Blobs kept">>, [BKF, BKB]},
                                            {<<"Blobs deleted">>, [BDF, BDB]}]}]},
            okjson(Resp, Req);
        E ->
            on_error(E, Req)
    end;

op(<<"GET">>, "/ddfs/ctrl/gc_status", Req) ->
    case ddfs_gc:gc_request(status) of
        {ok, not_running} ->
            okjson(<<"">>, Req);
        {ok, init_wait} ->
            okjson(<<"GC is waiting for the cluster to stabilize after startup.">>, Req);
        {ok, Phase} ->
            okjson(gc_phase_msg(Phase), Req);
        E ->
            on_error(E, Req)
    end;

op(<<"GET">>, "/ddfs/ctrl/gc_start", Req) ->
    case ddfs_gc:gc_request(start) of
        {ok, init_wait} ->
            okjson(<<"GC is waiting">>, Req);
        ok ->
            okjson(<<"GC has started">>, Req);
        E ->
            on_error(E, Req)
    end;

op(<<"GET">>, "/ddfs/ctrl/safe_gc_blacklist", Req) ->
    case ddfs_master:safe_gc_blacklist() of
        {ok, Nodes} ->
            Resp = [list_to_binary(disco:host(N)) || N <- Nodes],
            okjson(Resp, Req);
        E ->
            on_error(E, Req)
    end;

op(<<"GET">>, "/ddfs/new_blob/" ++ BlobName, Req) ->
    BlobK = list_to_integer(disco:get_setting("DDFS_BLOB_REPLICAS")),
    {Replicas, Req1} = cowboy_req:qs_val(<<"replicas">>, Req, false),
    K = case Replicas of
            false -> BlobK;
            {_, X} -> list_to_integer(X)
    end,
    {Exclude, Req2} = cowboy_req:qs_val(<<"exclude">>, Req1),
    Exc = parse_inclusion(Exclude),
    {Include, Req3} = cowboy_req:qs_val(<<"exclude">>, Req2),
    Inc = parse_inclusion(Include),
    case {Include, Inc} of
        {false, [_H|_T]} ->
            cowboy_req:reply(403, [], <<"This must not happen.">>, Req3);
        {false, _} ->
            new_blob(Req, BlobName, K, Inc, Exc);
        {_, [false]} ->
            cowboy_req:reply(403, [], <<"Requested Replica not found.">>, Req3);
        {_, [_H|_T]} ->
            new_blob(Req, BlobName, K, Inc, Exc)
    end;

op(<<"GET">>, "/ddfs/tags" ++ Prefix0, Req) ->
    Prefix = list_to_binary(string:strip(Prefix0, both, $/)),
    lager:info("in ddfs tags ~p ~p", [Prefix0, Prefix]),
    try case ddfs:tags(ddfs_master, Prefix) of
            {ok, Tags} -> okjson(Tags, Req);
            E -> on_error(E, Req)
        end
    catch K:V -> on_error({K,V}, Req)
    end;

op(<<"GET">>, "/ddfs/tag/" ++ TagAttrib, Req) ->
    {Tag, Attrib} = parse_tag_attribute(TagAttrib, all),
    {Token, Req1} = parse_auth_token(Req),
    case Attrib of
        unknown_attribute ->
            cowboy_req:reply(404, [], <<"Tag attribute not found.">>, Req1);
        {user, AttribName} when byte_size(AttribName) > ?MAX_TAG_ATTRIB_NAME_SIZE ->
            cowboy_req:reply(403, [], <<"Attribute name too big.">>, Req1);
        _ ->
            case ddfs:get_tag(ddfs_master, Tag, Attrib, Token) of
                {ok, TagData} ->
                    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], TagData, Req1);
                {missing, _} ->
                    cowboy_req:reply(404, [], <<"Tag not found.">>, Req1);
                invalid_name ->
                    cowboy_req:reply(403, [], <<"Invalid tag.">>, Req1);
                unknown_attribute ->
                    cowboy_req:reply(404, [], <<"Tag attribute not found.">>, Req1);
                E ->
                    on_error(E, Req1)
            end
    end;

op(<<"POST">>, "/ddfs/tag/" ++ Tag, Req) ->
    {Token, Req1} = parse_auth_token(Req),
    {QS, Req2} = cowboy_req:qs_vals(Req1),
    Opt = if_set(<<"update">>, QS, [nodup], []),
    process_payload(
      fun(Urls, _Size) ->
              case is_set(<<"delayed">>, QS) of
                  true ->
                      ddfs:update_tag_delayed(ddfs_master, Tag, Urls, Token, Opt);
                  false ->
                      ddfs:update_tag(ddfs_master, Tag, Urls, Token, Opt)
              end
      end, Req2);

op(<<"PUT">>, "/ddfs/tag/" ++ TagAttrib, Req) ->
    % for backward compatibility, return urls if no attribute is specified
    {Tag, Attrib} = parse_tag_attribute(TagAttrib, urls),
    {Token, Req1} = parse_auth_token(Req),
    case Attrib of
        unknown_attribute ->
            cowboy_req:reply(404, [], <<"Tag attribute not found.">>, Req1);
        {user, AttribName} when byte_size(AttribName) > ?MAX_TAG_ATTRIB_NAME_SIZE ->
            cowboy_req:reply(403, [], <<"Attribute name too big.">>, Req1);
        _ ->
            Op = fun(Value, Size) ->
                     case Attrib of
                         {user, _} when Size > ?MAX_TAG_ATTRIB_VALUE_SIZE ->
                             cowboy_req:reply(403, [], <<"Attribute value too big.">>, Req1);
                         _ ->
                             ddfs:replace_tag(ddfs_master, Tag, Attrib, Value,
                                              Token)
                     end
                 end,
            process_payload(Op, Req1)
    end;

op(<<"DELETE">>, "/ddfs/tag/" ++ TagAttrib, Req) ->
    {Tag, Attrib} = parse_tag_attribute(TagAttrib, all),
    {Token, Req1} = parse_auth_token(Req),
    case Attrib of
        unknown_attribute ->
            cowboy_req:reply(404, [], <<"Tag attribute not found.">>, Req1);
        all ->
            case ddfs:delete(ddfs_master, Tag, Token) of
                ok ->
                    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}],
                                     mochijson2:encode(<<"deleted">>), Req1);
                E ->
                    on_error(E, Req1)
            end;
        _ ->
            case ddfs:delete_attrib(ddfs_master, Tag, Attrib, Token) of
                ok ->
                    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}],
                                     mochijson2:encode(<<"deleted">>), Req1);
                E ->
                    on_error(E, Req1)
            end
    end;

op(<<"GET">>, Path, Req) ->
    DdfsRoot = disco:get_setting("DDFS_DATA"),
    ddfs_get:serve_ddfs_file(DdfsRoot, Path, Req);
op(<<"HEAD">>, Path, Req) ->
    DdfsRoot = disco:get_setting("DDFS_DATA"),
    ddfs_get:serve_ddfs_file(DdfsRoot, Path, Req);

op(_, _, Req) ->
    {ok, Req1} = cowboy_req:reply(404, [], <<>>, Req),
    Req1.

is_set(Flag, QS) ->
    case lists:keyfind(Flag, 1, QS) of
        {_, [_|_]} ->
            true;
        _ ->
            false
    end.

if_set(Flag, QS, True, False) ->
    case is_set(Flag, QS) of
        true ->
            True;
        false ->
            False
    end.

gc_phase_msg(start) ->
    <<"GC is initializing (phase start).">>;
gc_phase_msg(build_map) ->
    <<"GC is scanning DDFS (phase build_map).">>;
gc_phase_msg(map_wait) ->
    <<"GC is scanning DDFS (phase map_wait).">>;
gc_phase_msg(gc) ->
    <<"GC is performing garbage collection (phase gc).">>;
gc_phase_msg(rr_blobs) ->
    <<"GC is re-replicating blobs (phase rr_blobs).">>;
gc_phase_msg(rr_blobs_wait) ->
    <<"GC is re-replicating blobs (phase rr_blobs_wait).">>;
gc_phase_msg(rr_tags) ->
    <<"GC is re-replicating tags (phase rr_tags).">>.

-spec on_error(_, term()) -> _.
on_error(timeout, Req) ->
    cowboy_req:reply(503, [], <<"Temporary server error. Try again.">>, Req);
on_error({error, timeout}, Req) ->
    cowboy_req:reply(503, [], <<"Temporary server error. Try again.">>, Req);
on_error({error, unauthorized}, Req) ->
    cowboy_req:reply(401, [], <<"Incorrect or missing token.">>, Req);
on_error({error, invalid_name}, Req) ->
    cowboy_req:reply(403, [], <<"Invalid tag">>, Req);
on_error({error, invalid_url_object}, Req) ->
    cowboy_req:reply(403, [], <<"Invalid url object">>, Req);
on_error({error, invalid_attribute_value}, Req) ->
    cowboy_req:reply(403, [], <<"Invalid attribute key or value">>, Req);
on_error({error, too_many_attributes}, Req) ->
    cowboy_req:reply(403, [], <<"Too many attributes">>, Req);
on_error({error, unknown_attribute}, Req) ->
    cowboy_req:reply(404, [], <<"Tag attribute not found.">>, Req);
on_error({error, unknown_host}, Req) ->
    cowboy_req:reply(404, [], <<"Unknown host.">>, Req);
on_error({error, E}, Req) when is_atom(E) ->
    B = atom_to_binary(E, utf8),
    cowboy_req:reply(500, [], <<"Internal server error: ", B/binary>>, Req);
on_error(E, Req) ->
    Msg = ["Internal server error: ", io_lib:format("~p", [E])],
    cowboy_req:reply(500, [], list_to_binary(Msg), Req).

-spec okjson(json(), term()) -> {ok, term()}.
okjson(Data, Req) ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}],
                     mochijson2:encode(Data), Req).

-spec process_payload(fun(([binary()], non_neg_integer()) -> _), term()) -> _.
process_payload(Fun, Req) ->
    try  BinaryPayload = cowboy_req:body(Req, [{length, ?MAX_TAG_BODY_SIZE}]),
         Payload = try mochijson2:decode(BinaryPayload)
                   catch _:_ -> invalid end,
         case Payload of
             invalid ->
                 cowboy_req:reply(403, [], <<"Invalid request body.">>, Req);
             Value ->
                 case Fun(Value, byte_size(BinaryPayload)) of
                     {ok, Dst} -> okjson(Dst, Req);
                     E -> on_error(E, Req)
                 end
         end
    catch _:_ -> cowboy_req:reply(403, [], <<"Invalid request.">>, Req)
    end.

-spec parse_inclusion('false' | {'value', {_, string()}}) -> [node()].
parse_inclusion(false) -> [];
parse_inclusion({value, {_, Str}}) ->
    [disco:slave_safe(Host) || Host <- string:tokens(Str, ",")].

-spec new_blob(term(), string()|object_name(), non_neg_integer(), [node()], [node()]) ->
                      too_many_replicas | {ok, [nonempty_string()]}.
new_blob(Req, BlobName, K, Inc, Exc) ->
    case ddfs:new_blob(ddfs_master, BlobName, K, Inc, Exc) of
        {ok, Urls} when length(Urls) < K ->
            cowboy_req:reply(403, [], <<"Not enough nodes for replicas.">>, Req);
        too_many_replicas ->
            cowboy_req:reply(403, [], <<"Not enough nodes for replicas.">>, Req);
        invalid_name ->
            cowboy_req:reply(403, [], <<"Invalid prefix.">>, Req);
        {ok, Urls} ->
            okjson([list_to_binary(U) || U <- Urls], Req);
        E ->
            lager:warning("/ddfs/new_blob failed: ~p", [E]),
            on_error(E, Req)
    end.
