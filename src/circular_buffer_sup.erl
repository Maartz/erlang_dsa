-module(circular_buffer_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Size) ->
    supervisor:start_link(?MODULE, [Size]).

init([Size]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    
    ChildSpecs = [
        #{id => circular_buffer_manager,
          start => {circular_buffer_manager, start_link, [Size]},
          restart => permanent,
          type => worker},
          
        #{id => circular_buffer_store,
          start => {circular_buffer_store, start_link, [Size]},
          restart => permanent,
          type => worker}
    ],
    
    {ok, {SupFlags, ChildSpecs}}.