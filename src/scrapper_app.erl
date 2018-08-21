%% -----------------------------------------------------------------------------
%%
%% Web Scraper A.R.Tavanaei
%%
%% Copyright (c) 2015 
%%
%% -----------------------------------------------------------------------------

-module(scrapper_app).
-author('Per Andersson').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%% -----------------------------------------------------------------------------
%%
%% Application callbacks
%%
%% -----------------------------------------------------------------------------

-spec start(term(), term()) -> ok.
start(_StartType, _StartArgs) ->
    %% http client setup
    inets:start(httpc, [{profile, scrapper}]),
    httpc:set_options([{cookies, disabled}], scrapper),

    robotnik_sup:start_link().


-spec stop(term) -> ok.
stop(_State) ->
    inets:stop(http, [{profile, scrapper}]),
    ok.
