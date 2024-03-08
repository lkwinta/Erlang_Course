%%%-------------------------------------------------------------------
%%% @author Łukasz Kwinta
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. lut 2024 12:02
%%%-------------------------------------------------------------------
-module(myLists).
-author("Łukasz Kwinta").

%% API
-export([contains/2, duplicateElements/1, sumFloats/2]).

contains(_, []) -> false;
contains(H, [H | _])-> true;
contains(X, [_ | T]) -> contains(X, T).

duplicateElements([]) -> [];
duplicateElements([H | T]) ->
  [H, H | duplicateElements(T)].

sumFloats([], Sum) -> Sum;
sumFloats([H | T], Sum) when is_float(H)
  -> sumFloats(T, Sum + H);
sumFloats([_ | T], Sum)
  -> sumFloats(T, Sum).