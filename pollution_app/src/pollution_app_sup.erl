%%%-------------------------------------------------------------------
%% @doc pollution_app top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_app_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
gen_server_child_spec() -> #{
  id => pollution_server,                      % mandatory
  start => {pollution_gen_server, start_link, []}
}.

gen_statem_child_spec() -> #{
  id => pollution_statem,
  start => {pollution_value_collector_gen_statem, start_link, []}
}.

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 1},
    ChildSpecs = [gen_server_child_spec(), gen_statem_child_spec()],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
