-module(circular_buffer_manager).
-behaviour(gen_server).

-export([start_link/1, create/1, size/1, write/2, read/1, write_attempt/2]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).

-record(state, {
    store_pid :: pid(),
    size :: pos_integer(),
    overflow_policy = overwrite :: overwrite | reject
}).

%% Public API
start_link(Size) ->
    gen_server:start_link(?MODULE, [Size], []).

create(Size) ->
    circular_buffer_sup:start_link(Size).

size(ManagerPid) ->
    gen_server:call(ManagerPid, size).

write(ManagerPid, Item) ->
    gen_server:cast(ManagerPid, {write, Item}).

read(ManagerPid) ->
    gen_server:call(ManagerPid, read).

write_attempt(ManagerPid, Item) ->
    gen_server:call(ManagerPid, {write_attempt, Item}).

%% gen_server callbacks
init([Size]) ->
    {ok, StorePid} = circular_buffer_store:start_link(Size),
    {ok, #state{store_pid = StorePid, size = Size}}.

handle_call(size, _From, State) ->
    {reply, {ok, State#state.size}, State};
handle_call(read, _From, State) ->
    Result = circular_buffer_store:read(State#state.store_pid),
    {reply, Result, State};
handle_call({write_attempt, Item}, _From, State) ->
    case circular_buffer_store:check_space(State#state.store_pid) of
        {ok, has_space} ->
            ok = circular_buffer_store:write(State#state.store_pid, Item),
            {reply, ok, State};
        {ok, full} ->
            {reply, {error, full}, State}
    end;
handle_call(_, _From, State) ->
    {reply, {error, invalid_request}, State}.

handle_cast({write, Item}, State) ->
    case State#state.overflow_policy of
        overwrite ->
            circular_buffer_store:write_with_overwrite(State#state.store_pid, Item);
        reject ->
            case circular_buffer_store:check_space(State#state.store_pid) of
                {ok, has_space} ->
                    circular_buffer_store:write(State#state.store_pid, Item);
                {ok, full} ->
                    ok % Do nothing
            end
    end,
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, State) -> 
    {noreply, State}.