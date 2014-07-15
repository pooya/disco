-module(ets_keeper).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, give_ets2me/1]).

-include("config.hrl").
-include("gs_util.hrl").
-include("common_types.hrl").

-define(ETS_TABLE_VALID, (10 * ?DAY)).
-type state() :: disco_gbtree(atom(), {pid(), ets:tab(), erlang:timestamp()}).

-spec start_link() -> no_return().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> gs_init().
init([]) ->
    process_flag(trap_exit, true),
    {ok, gb_trees:empty()}.

-spec give_ets2me(atom()) -> ok.
give_ets2me(TabName) ->
    gen_server:call(?MODULE, {give_tab2pid, TabName, self()}).

-spec handle_call({give_tab2pid, atom(), pid()}, pid(), state()) -> gs_reply(ok).
handle_call({give_tab2pid, TabName, Pid}, _From, Tree) ->
    {reply, ok, do_give_tab2pid(TabName, Pid, Tree)}.

-spec do_give_tab2pid(atom(), pid(), state()) -> state().
do_give_tab2pid(TabName, Pid, Tree) ->
    case gb_trees:lookup(TabName, Tree) of
        none ->
            create_new_table(TabName, Pid, Tree);
        {value, {none, TabId, CreationTime}} ->
            Since = timer:now_diff(now(), CreationTime),
            case Since < ?ETS_TABLE_VALID of
                true ->
                    ets:delete(TabId),
                    create_new_table(TabName, Pid, Tree);
                false ->
                    ets:give_away(TabId, Pid, CreationTime),
                    gb_trees:update(TabName, {Pid, TabId, CreationTime}, Tree)
            end
    end.

-spec create_new_table(atom(), pid(), state()) -> state().
create_new_table(TabName, Pid, Tree) ->
    TabId = ets:new(TabName, [named_table, set, private]),
    ets:setopts(TabId, {heir, self(), {}}),
    gb_trees:insert(TabName, {Pid, TabId, now()}, Tree).

-spec handle_info({'ETS-TRANSFER', ets:tab(), pid(), {}}, state()) -> gs_noreply().
    handle_info({'ETS-TRANSFER', TabId, Pid, {}}, Tree) ->
    NewTree = case ets:info(TabId, name) of
        undefined ->
            Tree;
        TabName ->
            % make sure the Pid was the owner of the ets table and then update
            % this information in state.
            {Pid, TabId, CreationTime} = gb_trees:get(TabName, Tree),
            gb_trees:update(TabName, {none, TabId, CreationTime}, Tree)
    end,
    {noreply, NewTree}.

-spec handle_cast(term(), state()) -> gs_noreply().
handle_cast(_, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

