-module(edfs_app).

-behaviour(application).

-include("edfs.hrl").

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    start_listener(tcp),
    edfs_sup:start_link().

stop(_State) ->
    ok.

start_listener(tcp) ->
    PortConfig = case edfs_config:get(tcp_listener) of
                     {_IP, Port} ->
                         [{port, Port}];
                     random ->
                         []
                 end,
    TransportOpts = PortConfig ++ [{active, true}, {max_connections, infinity}],
    {ok, _} = ranch:start_listener(?EDFS_TCP_LISTENER, ?EDFS_TCP_ACCEPTORS,
                                       ranch_tcp, TransportOpts,
                                       edfs_vnode, []),
    Port1 = ranch:get_port(?EDFS_TCP_LISTENER),
    ?STDOUT("EDFS started on tcp port: ~p", [Port1]),
    ?LOG("EDFS started on tcp port: ~p", [Port1]).
