-module(cable_bench_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("log.hrl").
-include("priv.hrl").

-record(state, {}).

-export([start_link/0, start/3, start/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
  ?I("Server started"),

  StartConfig = ?Config(start, undefined),

  case StartConfig of
    List when is_list(List) -> [start(Host, Port, Channel, N) || {Host, Port, Channel, N} <- List];
    _ -> pass
  end,

  {ok, #state{}}.

start(Port, Channel, N) ->
  start("localhost", Port, Channel, N).

start(Host, Port, Channel, N) ->
  ChannelJSON = jsx:encode(#{ <<"channel">> => Channel }),
  gen_server:cast(?SERVER, {start, Host, Port, ChannelJSON, N}).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({start, Host, Port, Channel, N}, State) ->
  ?I({start_bench, Host, Port, Channel, N}),
  start_clients(Host, Port, Channel, N),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_clients(_Host, _Port, _Channel, 0) ->
  ?I("All workers have been launched"),
  ok;

start_clients(Host, Port, Channel, N) when (N rem 100) =:= 0 ->
  ?I({"Slow down, man!", N}),
  timer:sleep(2000),
  {ok, _Worker} = cable_bench_sup:start_client(Host, Port, Channel),
  start_clients(Host, Port, Channel, N - 1);

start_clients(Host, Port, Channel, N) ->
  {ok, _Worker} = cable_bench_sup:start_client(Host, Port, Channel),
  timer:sleep(100),
  start_clients(Host, Port, Channel, N - 1).
