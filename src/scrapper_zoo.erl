%% -----------------------------------------------------------------------------
%%
%% Web Scraper A.R.Tavanaei
%%
%% Copyright (c) 2015 
%%
%% -----------------------------------------------------------------------------

-module(scrapper_zoo).
-author('Per Andersson').

%% exported function
-export([spawn_robot/3]).

%% exported functions because they are called via ?MODULE
-export([do_spawn_robot/3,
         do_attack/3]).

%% -----------------------------------------------------------------------------
-spec spawn_robot(Module, Args, Opts) -> {ok, pid()}
    when Module :: atom(),
         Args   :: list(term()),
         Opts   :: proplists:proplist().
%% @doc
%%      Spawn a robot from the given module, arguments, and options.
%% @end
%% -----------------------------------------------------------------------------
spawn_robot(Module, Args, Opts) ->
    Pid = spawn(?MODULE, do_spawn_robot, [Module, Args, Opts]),
    {ok, Pid}.

-spec do_spawn_robot(Module, Args, Opts) -> {scrapper_zoo, reference(), Result}
    when Module :: atom(),
         Args   :: list(term()),
         Opts   :: proplists:proplist(),
         Result :: term().
do_spawn_robot(Module, Args, Opts) ->
    Caller = proplists:get_value(caller, Opts),
    Ref    = proplists:get_value(ref, Opts),

    %% spawn robot and send result to caller
    try Module:init(Args) of
        {ok, AttackPattern, State} ->
            Result = ?MODULE:do_attack(Module, AttackPattern, State),
            ok = Module:kill(),

            Caller ! {scrapper_zoo, Ref, Result};
        Error ->
            Caller ! {scrapper_zoo, Ref, Error}
    catch
        error:Error ->
            Caller ! {scrapper_zoo, Ref, Error}
    end.


%% -----------------------------------------------------------------------------
-spec do_attack(Module, Pattern, State) -> Result
    when Module   :: atom(),
         Pattern  :: list(atom()),
         State    :: term(),
         NewState :: term(),
         Result   :: {ok, robotnik:result()} | {next, NewState}
                   | {error, robotnik:reason_phrase()}.
%% @doc
%%      Execute the attack pattern for a robot scraper module.
%% @end
%% -----------------------------------------------------------------------------
do_attack(_Module, [], _State) ->
    {error, "Exhausted attack pattern without any successful attack."};
do_attack(Module, [Attack|Attacks], State) ->
    case Module:attack(Attack, State) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason};
        {next, NextState} ->
            ?MODULE:do_attack(Module, Attacks, NextState)
    end.
