-module(disco_file_server).

-include_lib("kernel/include/file.hrl").

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
    DocRoot = disco:get_setting("DISCO_WWW_ROOT"),
	{ok, Req, DocRoot}.

handle(Req, DocRoot) ->
    {Path, Req1} = cowboy_req:path(Req),
    Req2 = serve_file(Req1, binary_to_list(Path), DocRoot),
    {ok, Req2, DocRoot}.


serve_file(Req, "/", DocRoot) ->
    serve_file(Req, "/index.html", DocRoot);

serve_file(Req, Path, DocRoot) ->
    F = fun(Socket, Transport) ->
            Transport:sendfile(Socket, DocRoot ++ Path)
        end,
    Req1 = cowboy_req:set_resp_body_fun(F, Req),
    {ok, Req2} = cowboy_req:reply(200, Req1),
    Req2.

terminate(_Reason, _Req, _State) ->
	ok.
