%%%-------------------------------------------------------------------
%%% @author Łukasz Kwinta
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. lut 2024 11:54
%%%-------------------------------------------------------------------
-module(base_operations).
-author("Łukasz Kwinta").

%% API
-export([power/2]).

power(_, 0) ->
  1;
power(X, N) when N > 0 ->
  X * power(X, N - 1).

