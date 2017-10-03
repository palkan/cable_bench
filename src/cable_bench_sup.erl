-module(cable_bench_sup).
-behaviour(supervisor).

-export([start_link/0, start_client/3, stop_client/1]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client(Host, Port, Channel) ->
  supervisor:start_child(client_sup, [Host, Port, Channel]).

stop_client(Pid) ->
  supervisor:terminate_child(client_sup, Pid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([client]) ->
  {ok, {{simple_one_for_one, 5, 10}, [
    {undefined, {cable_bench_client, start_link, []},
      temporary, 2000, worker, [cable_bench_client]}
  ]}};

init([]) ->
  Children = [
    {
      client_sup,
      {
        supervisor,
        start_link,
        [{local, client_sup}, ?MODULE, [client]]
      },
      permanent,
      infinity,
      supervisor,
      []
    },
    ?CHILD(cable_bench_server, worker)
  ],
  {ok, {{one_for_one, 5, 10}, Children}}.
