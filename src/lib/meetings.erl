-module(meetings).

-export([can_attend/1, can_attend_basic/1]).

can_attend_basic(Intervals) ->
  MinutesSets = lists:map(fun([X,Y]) ->
                          Minutes = lists:seq(X, Y),
                          sets:from_list(Minutes)
                     end, Intervals),
  Ret = sets:intersection(MinutesSets),
  sets:is_empty(Ret),
  Ret.

can_attend(Intervals) ->
  FoldFn = fun([X, Y], {Min, Max, SegmentsSum}) when X < Y ->
             {min(X, Min), max(Y, Max), SegmentsSum + (Y - X + 1) }
           end,
  {Min, Max, SegmentsSum} = lists:foldl(FoldFn, {infinity, 0, 0}, Intervals),
  SegmentsSum =< Max - Min + 1.
