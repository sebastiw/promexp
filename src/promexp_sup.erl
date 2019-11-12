-module(promexp_sup).

-behaviour(supervisor).

-export([ start_link/0
        ]).

-export([ init/1
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{ strategy => one_for_all
                , intensity => 5
                , period => 1
                },
    ChildSpecs = [#{ id => promexp_registry
                   , start => {promexp_registry, start_link, []}
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.
