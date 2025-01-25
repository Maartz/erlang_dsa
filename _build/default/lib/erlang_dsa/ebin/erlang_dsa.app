{application,erlang_dsa,
             [{description,"Erlang DSA"},
              {vsn,"0.0.1"},
              {modules,[circular_buffer,circular_buffer_manager,
                        circular_buffer_store,circular_buffer_sup]},
              {registered,[circular_buffer_sup]},
              {applications,[kernel,stdlib]},
              {env,[]}]}.
