-module(working_days_util).

-include_lib("include/bank_days.hrl").

-export([working_days/3, month_days/2, year_days/1, build_index/1, print_index/2]).

working_days({_FromMonth, _FromDay} = From, {_ToMonth, _ToDay} = To, CacheMap) ->
  {IsFromFree, FromIndex} = maps:get(From, CacheMap),
  FromIndexFinal = from_index(IsFromFree, FromIndex),
  {_IsFree, ToIndex} = maps:get(To, CacheMap),
  ToIndex - FromIndexFinal + 1.

from_index(true, FromIndex)  -> FromIndex + 1;
from_index(false, FromIndex) -> FromIndex.

is_free_day(Year, Mon, Day) ->
  is_bank_holiday(Year, Mon, Day) orelse is_weekend(Year, Mon, Day).

is_weekend(Year, Mon, Day) -> calendar:day_of_the_week(Year, Mon, Day) > 5.

is_bank_holiday(Year, Mon, Day) ->
  BankDays = maps:get(Year, ?bank_days, #{}),
  maps:get({Mon, Day}, BankDays, false).

build_index(Year) ->
  CounterFn = fun({Mon, Day, true} = _DayTpl, {GlobalCounter, NewList})   ->
        % key-val tuple to build a map later
        TmpList = [{ {Mon, Day}, {true, GlobalCounter} } | NewList],
        {GlobalCounter, TmpList};
         ({Mon, Day, false} = _DayTpl, {GlobalCounter, NewList}) ->
        TmpList = [{ {Mon, Day}, {false, GlobalCounter + 1} } | NewList],
        {GlobalCounter + 1, TmpList}
      end,
  {_Counter, CountedDays} = lists:foldl(CounterFn, {0, []}, year_days(Year)),
  maps:from_list(CountedDays).

year_days(Year) ->
  lists:flatten([month_days(Year, Mon) || Mon <- lists:seq(1, 12)]).

month_days(Year, Month) ->
  DaysInMonth = calendar:last_day_of_the_month(Year, Month),
  [{Month, Day, is_free_day(Year, Month, Day)} || Day <- lists:seq(1, DaysInMonth)].

print_index(Year, Cache) ->
  lists:foreach(fun(Month) ->
                  Days = [ {Month, Day, maps:get({Month, Day}, Cache)} || Day <- lists:seq(1, calendar:last_day_of_the_month(Year, Month)) ],
                  io:format("Month #~p:~n~w~n~n",[Month, Days])
                end
               , lists:seq(1, 12)).