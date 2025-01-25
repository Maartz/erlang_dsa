-module(circular_buffer_store).
-behaviour(gen_server).

-export([start_link/1, check_space/1, write/2, read/1, write_with_overwrite/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    buffer :: array:array(),
    size :: pos_integer(),
    read_index = 0 :: non_neg_integer(),
    write_index = 0 :: non_neg_integer(),
    count = 0 :: non_neg_integer()
}).

%% Public API
start_link(Size) ->
    gen_server:start_link(?MODULE, [Size], []).

check_space(StorePid) ->
    gen_server:call(StorePid, check_space).

write(StorePid, Item) ->
    gen_server:call(StorePid, {write, Item}).

read(StorePid) ->
    gen_server:call(StorePid, read).

write_with_overwrite(StorePid, Item) ->
    gen_server:call(StorePid, {write_with_overwrite, Item}).

%% gen_server callbacks
init([Size]) ->
    {ok, #state{
        buffer = array:new(Size, {fixed, true}),
        size = Size
    }}.

handle_call(check_space, _From, #state{count = Count, size = Size} = State) ->
    Reply = case Count < Size of
        true -> {ok, has_space};
        false -> {ok, full}
    end,
    {reply, Reply, State};

handle_call({write, Item}, _From, State) ->
    NewState = do_write(Item, State, false),
    {reply, ok, NewState};

handle_call({write_with_overwrite, Item}, _From, State) ->
    NewState = do_write(Item, State, true),
    {reply, ok, NewState};

handle_call(read, _From, #state{count = 0} = State) ->
    {reply, {error, empty}, State};

handle_call(read, _From, State) ->
    #state{buffer = Buffer, read_index = RIdx, size = Size, count = Count} = State,
    Value = array:get(RIdx, Buffer),
    NewRIdx = (RIdx + 1) rem Size,
    NewState = State#state{read_index = NewRIdx, count = Count - 1},
    {reply, {ok, Value}, NewState};

handle_call(_, _From, State) ->
    {reply, {error, invalid_request}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
do_write(Item, State, AllowOverwrite) ->
    #state{buffer = Buffer, write_index = WIdx, read_index = RIdx, size = Size, count = Count} = State,
    NewBuffer = array:set(WIdx, Item, Buffer),
    NewWIdx = (WIdx + 1) rem Size,
    case Count < Size orelse AllowOverwrite of
        true when Count < Size -> % Normal write
            State#state{buffer = NewBuffer, write_index = NewWIdx, count = Count + 1};
        true -> % Overwrite
            NewRIdx = (RIdx + 1) rem Size,
            State#state{buffer = NewBuffer, write_index = NewWIdx, read_index = NewRIdx};
        false -> 
            State
    end.