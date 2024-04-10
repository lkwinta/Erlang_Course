%%%-------------------------------------------------------------------
%%% @author lukasz
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. mar 2024 11:41
%%%-------------------------------------------------------------------
-module(qsort).
-author("lukasz").

%% API
-export([qs/1, random_elems/3, compare_speeds/3]).

less_than(List, Arg) -> [X || X <- List, X < Arg].

grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([A]) -> [A];
qs([Pivot | Tail]) ->
  qs(less_than(Tail, Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail, Pivot)).

random_elems(N, Min, Max) ->
  [rand:uniform(Max - Min + 2) - 1 + Min || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  io:format("Fun1 time: ~p ms ~n", [Time1/1000]),
  io:format("Fun2 time: ~p ms ~n", [Time2/1000]).

