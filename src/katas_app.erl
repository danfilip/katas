%%%-------------------------------------------------------------------
%% @doc katas public API
%% @end
%%%-------------------------------------------------------------------

-module(katas_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    katas_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
