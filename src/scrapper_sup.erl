%% -----------------------------------------------------------------------------
%%
%% Web Scraper A.R.Tavanaei
%%
%% Copyright (c) 2015 
%%
%% -----------------------------------------------------------------------------

-module(scrapper_sup).
-author('Per Andersson').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% -----------------------------------------------------------------------------
%% Supervisor callbacks
%% -----------------------------------------------------------------------------
-spec init(list()) -> supervisor:child_spec().
init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
