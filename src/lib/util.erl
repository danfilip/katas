-module(util).

-include_lib("eunit/include/eunit.hrl").

-export([working_days/3, days/0, month_days/2, year_days/1, days_cache/1, print_cache/2]).

-define(months(Year),
          maps:from_list([{Month, calendar:last_day_of_the_month(Year, Month)} || Month <- lists:seq(1,12)])
       ).

-define(bank_days, #{ {1,1} => true
                    , {1,2} => true
                    , {1,24} => true
                    , {4,22} => true
                    , {4,24} => true
                    , {4,25} => true
                    , {5,1} => true
                    , {6,1} => true
                    , {6,12} => true
                    , {6,13} => true
                    , {8,15} => true
                    , {11,30} => true
                    , {12,1} => true
                    , {12,25} => true
                    , {12,26} => true
                    }).

working_days({_FromMonth, _FromDay} = From, {_ToMonth, _ToDay} = To, CacheMap) ->
  {IsFromFree, FromIndex} = maps:get(From, CacheMap),
  FromIndexFinal = from_index(IsFromFree, FromIndex),
  {_IsFree, ToIndex} = maps:get(To, CacheMap),
  ToIndex - FromIndexFinal + 1.

from_index(true, FromIndex)  -> FromIndex + 1;
from_index(false, FromIndex) -> FromIndex.

year_days(Year) ->
  lists:flatten([month_days(Year, Mon) || Mon <- lists:seq(1, 12)]).

month_days(Year, Mon) ->
  DaysInMonth = maps:get(Mon, ?months(Year)),
  [{Mon, Day, is_free_day(Year,Mon, Day)} || Day <- lists:seq(1, DaysInMonth)].

days() -> ?months(2022).

is_free_day(Year, Mon, Day) ->
  is_bank_holiday(Mon, Day) orelse is_weekend(Year, Mon, Day).

is_weekend(Year, Mon, Day) -> calendar:day_of_the_week(Year, Mon, Day) > 5.

is_bank_holiday(Mon, Day) -> maps:get({Mon, Day}, ?bank_days, false).

days_cache(Year) ->
  X = fun({Mon, Day, true} = DayTpl, {GlobalCounter, NewList} = Acc)   ->
        % key-val tuple to build a map later
        TmpList = [{ {Mon, Day}, {true, GlobalCounter} } | NewList],
        {GlobalCounter, TmpList};
         ({Mon, Day, false} = DayTpl, {GlobalCounter, NewList} = Acc) ->
        TmpList = [{ {Mon, Day}, {false, GlobalCounter + 1} } | NewList],
        {GlobalCounter + 1, TmpList}
      end,
  {Counter, CountedDays} = lists:foldl(X, {0, []}, year_days(Year)),
  maps:from_list(CountedDays).


print_cache(Year, Cache) ->
  lists:foreach(fun(Mon) ->
                  Days = [ {Mon, Day, maps:get({Mon, Day}, Cache)} || Day <- lists:seq(1, maps:get(Mon, ?months(Year))) ],
                  io:format("Month #~p:~n~w~n~n",[Mon, Days])
                end
               , lists:seq(1, 12)).
  % [ [ maps:get({Mon, Day}, Cache) || Day <- maps:get(Mon, ?months(Year))] || Mon <- lists:seq(1, 12)].

diff_test() ->
  Cache = util:days_cache(2022),
  5 = working_days({1, 1}, {1, 9}, Cache),
  13 = working_days({5, 31}, {6, 20}, Cache),
  42 = working_days({3, 1}, {4, 30}, Cache).

