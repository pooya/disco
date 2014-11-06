%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-include_lib("kernel/include/file.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
    File = filename:join([disco:get_setting("DISCO_WWW_ROOT"), "index.html"]),
    {ok, #file_info{size = Size}} = prim_file:read_file_info(File),
	{ok, Req, {File, Size}}.

handle(Req, {File, Size}) ->
    F = fun(Socket, Transport) ->
            Transport:sendfile(Socket, File)
        end,
    Req1 = cowboy_req:set_resp_body_fun(Size, F, Req), 
    {ok, Req2} = cowboy_req:reply(200, Req1),
    {ok, Req2, {File, Size}}.

terminate(_Reason, _Req, _State) ->
	ok.
