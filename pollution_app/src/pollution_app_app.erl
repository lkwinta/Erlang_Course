%%%-------------------------------------------------------------------
%% @doc pollution_app public API
%% @end
%%%-------------------------------------------------------------------

-module(pollution_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pollution_app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
