-module(circular_buffer).

-export([create/1, size/1, write/2, read/1, write_attempt/2, children/1]).

create(Size) ->
    {ok, SupPid} = circular_buffer_sup:start_link(Size),
    children(SupPid).

size(ManagerPid) ->
    circular_buffer_manager:size(ManagerPid).

write(ManagerPid, Item) ->
    circular_buffer_manager:write(ManagerPid, Item).

read(ManagerPid) ->
    circular_buffer_manager:read(ManagerPid).

write_attempt(ManagerPid, Item) ->
    circular_buffer_manager:write_attempt(ManagerPid, Item).

%% Add this function to retrieve the manager PID from the supervisor
children(SupPid) ->
    [ManagerPid] = [
        Pid || {Id, Pid, worker, _} <- supervisor:which_children(SupPid),
        Id =:= circular_buffer_manager
    ],
    ManagerPid.