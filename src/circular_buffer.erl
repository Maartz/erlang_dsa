%%% @doc A circular buffer implementation using gen_server behavior.
%%%
%%% This module provides a thread-safe circular buffer with overwrite capabilities
%%% when full. It supports both blocking and non-blocking write operations.
-module(circular_buffer).

-behaviour(gen_server).

%% API
-export([create/1, size/1, write/2, read/1, write_attempt/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% Types
-type buffer_ref() :: pid().         % Reference to buffer instance
-type buffer_size() :: pos_integer().% Buffer capacity (positive integer)
-type buffer_item() :: term().       % Any Erlang term can be stored

-record(state,
        {buffer :: array:array(),         % Internal array storage
         size :: buffer_size(),           % Maximum buffer capacity
         read_index = 0 :: non_neg_integer(), % Current read position
         write_index = 0 :: non_neg_integer(),% Current write position
         count = 0 :: non_neg_integer()}).   % Number of items currently stored

-type state() :: #state{}.

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Creates a new circular buffer with specified size
%% @param Size Positive integer specifying buffer capacity
%% @returns Pid of the newly created buffer process
-spec create(Size :: buffer_size()) -> buffer_ref().
create(Size) ->
    {ok, Pid} = gen_server:start_link(?MODULE, Size, []),
    Pid.

%% @doc Gets the maximum capacity of the buffer
%% @param Buffer Pid of the buffer instance
%% @returns {ok, Size} where Size is the buffer capacity
-spec size(Buffer :: buffer_ref()) -> {ok, buffer_size()}.
size(Buffer) ->
    gen_server:call(Buffer, size).

%% @doc Writes an item to the buffer (overwrites oldest item if full)
%% @param Buffer Pid of the buffer instance
%% @param Item The item to be stored in the buffer
%% @returns Always returns ok, even when overwriting
-spec write(Buffer :: buffer_ref(), Item :: buffer_item()) -> ok.
write(Buffer, Item) ->
    gen_server:cast(Buffer, {write, Item}).

%% @doc Reads the oldest item from the buffer
%% @param Buffer Pid of the buffer instance
%% @returns {ok, Item} if items exist, {error, empty} if buffer is empty
-spec read(Buffer :: buffer_ref()) -> {ok, buffer_item()} | {error, empty}.
read(Buffer) ->
    gen_server:call(Buffer, read).

%% @doc Attempts to write an item without overwriting existing data
%% @param Buffer Pid of the buffer instance
%% @param Item The item to be stored in the buffer
%% @returns ok if successful, {error, full} if buffer is full
-spec write_attempt(Buffer :: buffer_ref(), Item :: buffer_item()) -> ok | {error, full}.
write_attempt(Buffer, Item) ->
    gen_server:call(Buffer, {write_attempt, Item}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the buffer with fixed-size array storage
-spec init(Size :: buffer_size()) -> {ok, state()} | {error, badarg}.
init(Size) when Size > 0 ->
    {ok,
     #state{buffer = array:new(Size, {fixed, true}), % Pre-allocated fixed-size array
            size = Size}};
init(_) ->
    {error, badarg}.

%% @private
%% @doc Handles synchronous calls (size, read, write_attempt)
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: state()) ->
                     {reply, term(), state()}.
handle_call(size, _From, #state{size = Size} = State) ->
    {reply, {ok, Size}, State};
handle_call(read, _From, #state{count = 0} = State) ->
    {reply, {error, empty}, State};
handle_call(read,
            _From,
            #state{buffer = Buffer,
                   read_index = RIdx,
                   size = Size,
                   count = Count} =
                State) ->
    Value = array:get(RIdx, Buffer),
    NewRIdx = (RIdx + 1) rem Size,
    NewState = State#state{read_index = NewRIdx, count = Count - 1},
    {reply, {ok, Value}, NewState};
handle_call({write_attempt, Item}, _From, #state{count = Count, size = Size} = State)
    when Count < Size ->
    NewState = do_write(Item, State),
    {reply, ok, NewState};
handle_call({write_attempt, _Item}, _From, State) ->
    {reply, {error, full}, State};
handle_call(_, _From, State) ->
    {reply, {error, invalid_request}, State}.

%% @private
%% @doc Handles asynchronous writes that overwrite when full
-spec handle_cast(Request :: term(), State :: state()) -> {noreply, state()}.
handle_cast({write, Item}, State) ->
    NewState = do_write(Item, State),
    {noreply, NewState};
handle_cast(_, State) ->
    {noreply, State}.

%% @private
%% @doc Handles system messages (not used for this implementation)
-spec handle_info(Info :: term(), State :: state()) -> {noreply, state()}.
handle_info(_, State) ->
    {noreply, State}.

%% @private
%% @doc Cleanup on termination (no special cleanup needed)
-spec terminate(Reason :: term(), State :: state()) -> ok.
terminate(_, _) ->
    ok.

%% @private
%% @doc Handle code changes (no special handling needed)
-spec code_change(OldVsn :: term(), State :: state(), Extra :: term()) -> {ok, state()}.
code_change(_, State, _) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Internal write operation handling both normal and overwrite cases
%% When buffer is full:
%% 1. Overwrite oldest item (at read_index)
%% 2. Advance both write_index and read_index
%% When buffer has space:
%% 1. Store item at write_index
%% 2. Advance write_index
%% 3. Increment count
-spec do_write(Item :: buffer_item(), State :: state()) -> state().
do_write(Item,
         #state{buffer = Buffer,
                write_index = WIdx,
                read_index = RIdx,
                size = Size,
                count = Count} =
             State) ->
    NewBuffer = array:set(WIdx, Item, Buffer),
    NewWIdx = (WIdx + 1) rem Size,
    case Count < Size of
        true -> % Normal write
            State#state{buffer = NewBuffer,
                        write_index = NewWIdx,
                        count = Count + 1};
        false -> % Overwrite mode
            NewRIdx = (RIdx + 1) rem Size,
            State#state{buffer = NewBuffer,
                        write_index = NewWIdx,
                        read_index = NewRIdx}
    end.
