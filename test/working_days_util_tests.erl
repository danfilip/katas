-module(working_days_util_tests).

-include_lib("eunit/include/eunit.hrl").

days_diff_test() ->
  Index = working_days_util:build_index(2022),
  5 = working_days_util:working_days({1, 1}, {1, 9}, Index),
  13 = working_days_util:working_days({5, 31}, {6, 20}, Index),
  42 = working_days_util:working_days({3, 1}, {4, 30}, Index).
% timer:tc(fun() -> lists:foreach(fun(X) -> super_calendar:working_days({3, 1}, {4, 30}) end, lists:seq(1, 1000000)) end).