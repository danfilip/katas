-module(meetings_tests).

-include_lib("eunit/include/eunit.hrl").

attendance_test() ->
  ?assertNot(meetings:can_attend([[0,40],[5,10],[15,50]])),
  ?assert(meetings:can_attend([[7,10],[2,4],[20,25],[15,18]])),
  ?assert(meetings:can_attend([[0,10],[11,20],[21,40],[39,59]])).