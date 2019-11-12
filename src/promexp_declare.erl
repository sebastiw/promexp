-module(promexp_declare).

-export([ test/1
        ]).

-include_lib("kernel/include/logger.hrl").
-include("promexp.hrl").

-type metric_type() :: non_histogram_type() | histogram_type().
-type name() :: atom() | string().
-type label() :: atom() | string().
-type bucket() :: integer().
-type help() :: string().

-type non_histogram_type() :: counter | gauge.
-type histogram_type() :: histogram.

-type definition() :: #{type := metric_type(), name := name(),
                        help => string(), labels => list(label()),
                        buckets => list()}
                    | {metric_type(), name(), help()}
                    | {metric_type(), name(), help(), list(label()) | list(bucket())}.

-type valid_definition() :: #counter{} | #gauge{} | #histogram{}.

-spec test(definition() | list(definition())) ->
                  list(valid_definition()) | {error, term()}.

test(Metric) when is_tuple(Metric) ->
    test([Metric]);
test(#{type := _} = Metric) ->
    test([Metric]);
test(Metrics) when is_list(Metrics) ->
    ?LOG_DEBUG("Declaring ~B metrics", [length(Metrics)]),
    Ms = lists:map(fun safe_convert_to_record/1, Metrics),
    case [M || {false, M} <- Ms] of
        [] ->
            check_errors(Ms);
        NotConverted ->
            ?LOG_ERROR("Not recognized format ~1000p", [NotConverted]),
            {error, {not_declared, NotConverted}}
    end.

safe_convert_to_record(#{type := counter, name := N, help := H} = M) ->
    #counter{ name = N
            , help = H
            , labels = maps:get(labels, M, [])
            };
safe_convert_to_record({counter, N, H}) ->
    #counter{ name = N
            , help = H
            };
safe_convert_to_record({counter, N, H, L}) ->
    #counter{ name = N
            , help = H
            , labels = L
            };
safe_convert_to_record(#{type := gauge, name := N, help := H} = M) ->
    #gauge{ name = N
          , help = H
          , labels = maps:get(labels, M, [])
          };
safe_convert_to_record({gauge, N, H}) ->
    #gauge{ name = N
          , help = H
          };
safe_convert_to_record({gauge, N, H, L}) ->
    #gauge{ name = N
          , help = H
          , labels = L
          };
safe_convert_to_record(#{type := histogram, name := N, help := H, buckets := B} = M) ->
    #histogram{ name = N
              , help = H
              , buckets = B
              , labels = maps:get(labels, M, [])
              };
safe_convert_to_record({histogram, N, H, B}) ->
    #histogram{ name = N
              , help = H
              , buckets = B
              };
safe_convert_to_record({histogram, N, H, B, L}) ->
    #histogram{ name = N
              , help = H
              , buckets = B
              , labels = L
              };
safe_convert_to_record(M) ->
    {false, M}.

check_errors(Ms) ->
    case [E || {error, E} <- lists:map(fun try_declare/1, Ms)] of
        [] ->
            Ms;
        Errors ->
            ?LOG_ERROR("Could not declare ~1000p", [Errors]),
            {error, {multiple_errors, Errors}}
    end.

try_declare(#counter{name = N, help = H, labels = L}) ->
    Spec = [{name, N}, {help, H}, {labels, L}],
    try
        prometheus_counter:declare(Spec),
        prometheus_counter:remove(N, L)
    catch
        E:R:S ->
            logger:debug(S),
            {error, {prometheus_error, N, E, R}}
    end;
try_declare(#gauge{name = N, help = H, labels = L}) ->
    Spec = [{name, N}, {help, H}, {labels, L}],
    try
        prometheus_gauge:declare(Spec),
        prometheus_gauge:remove(N, L)
    catch
        E:R:S ->
            logger:debug(S),
            {error, {prometheus_error, N, E, R}}
    end;
try_declare(#histogram{name = N, help = H, buckets = B, labels = L}) ->
    Spec = [{name, N}, {help, H}, {buckets, B}, {labels, L}],
    try
        prometheus_histogram:declare(Spec),
        prometheus_histogram:remove(N, L)
    catch
        E:R:S ->
            logger:debug(S),
            {error, {prometheus_error, N, E, R}}
    end.
