%%%-------------------------------------------------------------------
%%% @author lukasz
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. kwi 2024 11:39
%%%-------------------------------------------------------------------
-module(pingpong).
-author("lukasz").

%% API
-export([start/0, play/1, stop/0]).

start() ->
  register(ping, spawn(fun () -> ping_loop(0) end)),
  register(pong, spawn(fun () -> pong_loop() end)),
  %% NEEDS EXPORT:
  %% register(pong, spawn(?MODULE, pong_loop, [])),

  ok.

play(N) ->
  whereis(ping) ! N.

stop() ->
  whereis(ping) ! stop,
  whereis(pong) ! stop.

ping_loop(Sum) ->
  receive
    stop ->
      io:format("Stopping ping ~n"),
      ok;
    0 -> ping_loop(Sum);
    N ->
      io:format("Ping send to pong: Sum: ~p ~n", [Sum]),
      timer:sleep(100),
      whereis(pong) ! N - 1,
      ping_loop(Sum + N)
  after
    20000 ->
      io:format("Ping timeout ~n"),
      ok
  end.

pong_loop() ->
  receive
    stop ->
      io:format("Stopping pong ~n"),
      ok;
    0 -> pong_loop();
    N ->
      io:format("Pong send to ping ~n"),
      timer:sleep(100),
      whereis(ping) ! N - 1,
      pong_loop()
  after
    20000 ->
      io:format("Pong timeout ~n"),
      ok
  end.