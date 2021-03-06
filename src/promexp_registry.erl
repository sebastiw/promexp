-module(promexp_registry).

-behaviour(gen_server).

-export([ start_link/0
        , register_metrics/2
        , register_metrics/3
        , collect/0
        , extract_one/2
        , deregister_all/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-include("promexp.hrl").

-record(metric, { type :: counter | gauge | histogram
                , name :: metric_name()
                , help :: string()
                , labels :: [atom()]
                , buckets = undefined :: [number()] | undefined
                , collect_fun :: reference()
                , extract_fun :: reference()
                }).

-record(s, { metrics = [] :: [#metric{}]
           , collectors = [] :: [{reference(), fun()}]
           , extractors = [] :: [{reference(), fun()}]
           }).

-define(DEFAULT_EXTRACT_FUN,
        fun (M, CollectMap) when is_map(CollectMap) ->
                case maps:get(M, CollectMap, undefined) of
                    V when is_number(V) ->
                        #{[] => V};
                    Map when is_map(Map) ->
                        maps:to_list(Map);
                    undefined ->
                        undefined
                end;
            (_M, Else) ->
                logger:error({promexp_collect_is_not_map, Else}),
                undefined
        end).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_metrics(CollectFun, Metrics) ->
    register_metrics(CollectFun, ?DEFAULT_EXTRACT_FUN, Metrics).

register_metrics(CollectFun, ExtractFun, Metrics) ->
    gen_server:call(?MODULE, {register, CollectFun, ExtractFun, Metrics}).

collect() ->
    gen_server:call(?MODULE, collect).

extract_one(MetricName, CollectedData) ->
    gen_server:call(?MODULE, {extract_one, MetricName, CollectedData}).

deregister_all() ->
    gen_server:call(?MODULE, deregister_all).

init(_) ->
    erlang:process_flag(trap_exit, true),
    {ok, #s{}}.

handle_call({register, CollectFun, ExtractFun, Metrics}, _, State) ->
    case promexp_declare:test(Metrics) of
        {error, Err} ->
            {reply, Err, State};
        Ms ->
            {Reply, NewState} = add_metrics(State, CollectFun, ExtractFun, Ms),
            {reply, Reply, NewState}
    end;
handle_call(collect, _, State) ->
    Cs = collect_all(State#s.collectors),
    {reply, Cs, State};
handle_call(collect_and_extract, _, State) ->
    Extractors = maps:from_list(State#s.extractors),
    Cs = collect_all(State#s.collectors),
    Res = extract_all(State#s.metrics, Extractors, Cs),
    {reply, Res, State};
handle_call({extract_one, MetricName, CollectData}, _, State) ->
    case lists:keyfind(MetricName, #metric.name, State#s.metrics) of
        false ->
            logger:error({promexp_not_declared, MetricName}),
            {reply, undefined, State};
        Metric ->
            Extractors = maps:from_list(State#s.extractors),
            Res = extract_one(Metric, Extractors, CollectData),
            {reply, Res, State}
    end;
handle_call(deregister_all, _, _) ->
    %% TODO: Also deregister from prometheus?
    {reply, ok, #s{}}.

handle_cast(_What, State) ->
    {noreply, State}.

add_metrics(State0, CollectFun, ExtractFun, Metrics) ->
    {CRef, State1} = add_collector(State0, CollectFun),
    {ERef, State2} = add_extractor(State1, ExtractFun),
    {L, NewState} = do_add_metrics(State2, CRef, ERef, Metrics),
    case lists:filtermap(fun (ok) -> false; (E) -> {true, E} end, L) of
        [] ->
            {ok, NewState};
        Errors ->
            {{error, Errors}, State0}
    end.

add_collector(State, CollectFun) ->
    case lists:keyfind(CollectFun, 2, State#s.collectors) of
        false ->
            R = make_ref(),
            {R, State#s{collectors = [{R, CollectFun}|State#s.collectors]}};
        {R, _} ->
            {R, State}
    end.

add_extractor(State, ExtractFun) ->
    case lists:keyfind(ExtractFun, 2, State#s.extractors) of
        false ->
            R = make_ref(),
            NewState = State#s{extractors = [{R, ExtractFun}|State#s.extractors]},
            {R, NewState};
        {R, _} ->
            {R, State}
    end.

do_add_metrics(State, CRef, ERef, Metrics) ->
    %% Check if metrics already added
    %% -> ok if same CRef + ERef
    %% -> else fail
    Ms = convert_metrics(Metrics, CRef, ERef),
    lists:mapfoldl(fun try_add_metric/2, State, Ms).

try_add_metric(M, State) ->
    case lists:keyfind(M#metric.name, #metric.name, State#s.metrics) of
        false ->
            {ok, State#s{metrics = [M|State#s.metrics]}};
        M ->                                    % Exact same
            {ok, State};
        Else ->                                 % Something differs
            {{promexp_added_with_different_values, M, Else}, State}
    end.

convert_metrics([], _, _) ->
    [];
convert_metrics([C|Metrics], CRef, ERef) ->
    M = convert_metric(C),
    [M#metric{ collect_fun = CRef
             , extract_fun = ERef
             }
     |convert_metrics(Metrics, CRef, ERef)].

convert_metric(#counter{name = N, help = H, labels = L}) ->
    #metric{ type = counter
           , name = N
           , help = H
           , labels = L
           };
convert_metric(#gauge{name = N, help = H, labels = L}) ->
    #metric{ type = gauge
           , name = N
           , help = H
           , labels = L
           };
convert_metric(#histogram{name = N, help = H, labels = L, buckets = B}) ->
    #metric{ type = histogram
           , name = N
           , help = H
           , labels = L
           , buckets = B
           }.

collect_all(Collectors) ->
    lists:foldl(fun ({Ref, ColFun}, Acc) ->
                        Acc#{Ref => collect_data(ColFun)}
                end,
                #{},
                Collectors).

collect_data(Fun) ->
    %% TODO: Spawn separately and wait for results
    Fun().

extract_all(Metrics, Extractors, CollectedResults) ->
    lists:map(fun (M) ->
                      extract_one(M, Extractors, CollectedResults)
              end,
              Metrics).

extract_one(M, Extractors, CollectedResults) ->
    ColRes = maps:get(M#metric.collect_fun, CollectedResults),
    ExtFun = maps:get(M#metric.extract_fun, Extractors),
    %% TODO: Add prometheus_model_helper:type_metrics
    extract_data(M, ExtFun, ColRes).

extract_data(Metric, ExtractFun, Result) ->
    try
        ValueMap = ExtractFun(Metric#metric.name, Result),
        [{zip_labels(Metric#metric.labels, Labels), V}
         || {Labels, V} <- maps:to_list(ValueMap)]
    catch
        E:R:S ->
            logger:error({promexp_extract_failed, E, R, S})
    end.

zip_labels(LabelNames, Labels) when is_tuple(Labels) ->
    zip_labels(LabelNames, tuple_to_list(Labels));
zip_labels(LabelNames, Labels) when is_list(Labels) ->
    lists:zip(LabelNames, Labels).
