-module(promexp_app).

-behaviour(application).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
                 [{'_', [{"/" ++ promexp_config:endpoint(), promexp_handler, []}]}]),
    {ok, _} = cowboy:start_clear( promexp_listener
                                , [{port, promexp_config:port()}]
                                , #{ env => #{dispatch => Dispatch}
                                   , metrics_callback => fun metrics_callback/1
                                   , stream_handlers => [ cowboy_metrics_h
                                                        , cowboy_stream_h
                                                        ]
                                   }),
    promexp_sup:start_link().

stop(_State) ->
    ok.

%% TODO!!
metrics_callback(_) ->
    ok.

%% #{informational => [],pid => <0.249.0>,
%%   procs =>
%%       #{<0.250.0> =>
%%             #{exit => -576460742528664953,reason => normal,
%%               spawn => -576460742528824529}},
%%   reason => normal,ref => promexp_listener,
%%   req =>
%%       #{body_length => 0,cert => undefined,has_body => false,
%%         headers =>
%%             #{<<"accept">> => <<"*/*">>,<<"host">> => <<"localhost:8081">>,
%%               <<"user-agent">> => <<"curl/7.66.0">>},
%%         host => <<"localhost">>,method => <<"GET">>,path => <<"/metrics">>,
%%         peer => {{127,0,0,1},46324},
%%         pid => <0.249.0>,port => 8081,qs => <<>>,ref => promexp_listener,
%%         scheme => <<"http">>,
%%         sock => {{127,0,0,1},8081},
%%         streamid => 1,version => 'HTTP/1.1'},
%%   req_body_end => undefined,req_body_length => 0,req_body_start => undefined,
%%   req_end => -576460742528650548,req_start => -576460742528840979,
%%   resp_body_length => 12,resp_end => -576460742528723699,
%%   resp_headers =>
%%       #{<<"content-length">> => <<"12">>,
%%         <<"content-type">> => <<"text/plain">>,
%%         <<"date">> => <<"Tue, 15 Oct 2019 12:16:51 GMT">>,
%%         <<"server">> => <<"Cowboy">>},
%%   resp_start => -576460742528723699,resp_status => 200,streamid => 1}
