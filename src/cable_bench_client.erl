-module(cable_bench_client).
-behaviour(gen_server).
-define(WELCOME, <<"{\"type\":\"welcome\"}">>).
-include("log.hrl").

-record(state, {
  host ::string(),
  port ::non_neg_integer(),
  channel ::binary()
}).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Host, Port, Channel) ->
  gen_server:start_link(?MODULE, [Host, Port, Channel], []).

init([Host, Port, Channel]) ->
  self() ! start,
  {ok, #state{host = Host, port = Port, channel = Channel}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(start, #state{host = Host, port = Port, channel = Channel} = State) ->
  ConnPid = gun_open(Host, Port),
  gun:ws_upgrade(ConnPid, "/cable"),
  {gun_ws_upgrade, ConnPid, ok, _Headers} = message(),
  {gun_ws, ConnPid, {text, ?WELCOME}} = message(),
  gun:ws_send(ConnPid, {text, subscribe_msg(Channel)}),
  Confirm = subscribe_confirm(Channel),
  ?D({wait_confirm, Confirm}),
  {gun_ws, ConnPid, {text, Confirm}} = message(),
  ?D({subscribed, Channel}),
  {noreply, State};

handle_info({gun_error, ConnPid, StreamRef, Reason} = Error, State) ->
  ?E(Error),
  {stop, State};

handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

gun_open(Host, Port) ->
  {ok, ConnPid} = gun:open(Host, Port),
  {gun_up, ConnPid, http} = message(),
  ConnPid.

message() ->
  message(15000).

message(Timeout) ->
  receive
    {gun_ws, _, {text, <<"{\"type\":\"ping\"", _Rest/binary>>}} ->
      message(Timeout);
    Message ->
      ?D({message, Message}),
      Message
  after Timeout ->
    timeout
  end.

subscribe_msg(Channel) ->
  jsx:encode(#{ command => subscribe, identifier => Channel }).

subscribe_confirm(Channel) ->
  jsx:encode(#{ type => confirm_subscription, identifier => Channel }).
