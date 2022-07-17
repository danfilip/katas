-module(super_calendar).

-behaviour(gen_server).

-export([start_link/0, working_days/2]).

-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {days_index :: map() }).

start_link() ->
  {{CurrentYear, _, _},{_, _, _}} = calendar:local_time(),
  Index = working_days_util:build_index(CurrentYear),
  gen_server:start_link({local, super_calendar}, ?MODULE, Index, []).

init(DaysIndex) ->
  {ok, #state{days_index = DaysIndex}}.

working_days(From, To) ->
  % todo: make sure start was called first
  gen_server:call(super_calendar, {working_days, From, To}).

handle_call({working_days, From, To}, _From, #state{days_index = Index} = State) ->
  Days = working_days_util:working_days(From, To, Index),
  {reply, Days, State}.

handle_cast({working_days, From, To}, #state{days_index = Index} = State) ->
  Days = working_days_util:working_days(From, To, Index),
  {reply, Days, State}.