-module(promexp).

-export([ start/0
        , stop/0
        ]).

start() ->
    application:ensure_all_started(?MODULE).

stop() ->
    application:stop(?MODULE).
