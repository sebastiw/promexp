-module(promexp_collector).

-behaviour(prometheus_collector).

-export([ collect_mf/2
        , deregister_cleanup/1
        , collect_metrics/2
        ]).

collect_mf(Registry, Callback) ->
    logger:debug("Got call ~s:collect_mf(~p, ~p)", [?MODULE, Registry, Callback]),
    promexp_registry:collect().

deregister_cleanup(_Registry) ->
    promexp_registry:deregister_all().

collect_metrics(MetricName, Data) ->
    promexp_registry:extract_one(MetricName, Data).
