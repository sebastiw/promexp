-module(promexp_handler).

-behaviour(cowboy_handler).

-export([ init/2
        , terminate/3
        ]).

init(Req0, State) ->
    Manual = promexp_config:manual_scrape(),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           collect_metrics(Manual),
                           Req0),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

collect_metrics(manual) ->
    promexp_registry:collect(),
    prometheus_text_format:format(promexp_registry);
collect_metrics(_) ->
    prometheus_text_format:format(promexp_registry).
