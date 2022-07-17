-module(meetings).

-export([can_attend/1, can_attend_basic/1]).

can_attend_basic(Input) ->
  Input2 = lists:map(fun([X,Y]) -> lists:seq(X, Y) end, Input),
  Sets = lists:map(fun(L) -> sets:from_list(L) end, Input2),
  Ret = sets:intersection(Sets),
  sets:is_empty(Ret).

can_attend(List) ->
  FoldFn = fun([X, Y], {Min, Max, SegmentSum}) when X < Y ->
             {min(X, Min), max(Y, Max), SegmentSum + (Y - X) }
           end,
  {Min, Max, SegmentSum} = lists:foldl(FoldFn, {0, 0, 0}, List),
  SegmentSum =< Max - Min.
