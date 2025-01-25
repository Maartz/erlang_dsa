-module(circular_buffer_translated).
-behaviour(gen_server).

%% API
-export([start_link/1, read/1, write/2, overwrite/2, clear/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal functions
-export([insert_or_update/2]).

start_link(Capacity) ->
    gen_server:start_link(?MODULE, Capacity, []).

read(Buffer) ->
    gen_server:call(Buffer, read).

write(Buffer, Item) ->
    gen_server:call(Buffer, {write, Item}).

overwrite(Buffer, Item) ->
    gen_server:call(Buffer, {overwrite, Item}).

clear(Buffer) ->
    gen_server:cast(Buffer, clear).

init(Capacity) ->
    {ok, {Capacity, []}}.

handle_call(read, _From, {_, []}) ->
    {reply, {error, empty}, []};
handle_call(read, _From, {Capacity, [Item | List]}) ->
    {reply, {ok, Item}, {Capacity, List}};
handle_call({write, Item}, _From, {Capacity, List}) when length(List) < Capacity ->
    {reply, ok, {Capacity, insert_or_update(List, Item)}};
handle_call({write, _Item}, _From, State) ->
    {reply, {error, full}, State};
handle_call({overwrite, Item}, _From, {Capacity, List}) when length(List) < Capacity ->
    {reply, ok, {Capacity, insert_or_update(List, Item)}};
handle_call({overwrite, Item}, _From, {Capacity, [_ | List]}) ->
    {reply, ok, {Capacity, insert_or_update(List, Item)}}.

handle_cast(clear, {Capacity, _}) ->
    {noreply, {Capacity, []}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

insert_or_update([], Item) ->
    [Item];
insert_or_update([CurrentItem | []], Item) ->
    [CurrentItem | [Item]];
insert_or_update([CurrentItem | List], Item) ->
    [CurrentItem | insert_or_update(List, Item)].

