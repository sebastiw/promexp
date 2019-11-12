-module(promexp_config).

-export([ port/0
        , endpoint/0
        , manual_scrape/0
        , scrape_interval/0
        , scrape_timeout/0
        , collect_data_retries/0
        ]).

port() ->
    is_positive(application:get_env(?MODULE, port, 8081)).

endpoint() ->
    is_endpoint(application:get_env(?MODULE, endpoint, "metrics")).

manual_scrape() ->
    is_bool(application:get_env(?MODULE, manual_scrape, false)).

scrape_interval() ->
    is_positive(application:get_env(?MODULE, scrape_interval, 5000)).

scrape_timeout() ->
    is_positive(application:get_env(?MODULE, scrape_timeout, 500)).

collect_data_retries() ->
    is_positive(application:get_env(?MODULE, collect_retries, 5)).

is_bool(B) when is_boolean(B) ->
    B.

is_positive(I) when is_integer(I), I > 0 ->
    I.

is_endpoint("/" ++ S)  ->
    S;
is_endpoint(S) when is_list(S) ->
    S.
