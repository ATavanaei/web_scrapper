%% -----------------------------------------------------------------------------
%%
%% Web Scraper A.R.Tavanaei
%%
%% Copyright (c) 2015 
%%
%% -----------------------------------------------------------------------------
-module(gen_scrapper).
-author('Per Andersson').

-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> proplists:proplist() | undefined.
behaviour_info(callbacks) ->
    [
     %% Args -> {ok, Attackpattern, State}
     {init, 1},
     %% -> ok
     {kill, 0},
     %% (Attack, State) -> {ok, Result} | {next, NextState} | {error, Reason}
     {attack, 2}
    ];
behaviour_info(_) ->
    undefined.
