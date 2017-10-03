-module(cable_bench_app).
-behaviour(application).

-define(APPS, [lager, ulitos, jsx, gun]).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  ulitos_app:ensure_started(?APPS),
  cable_bench_sup:start_link().

stop(_State) ->
    ok.
