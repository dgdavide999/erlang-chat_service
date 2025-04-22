-module(miniclip_test_tests).
-include_lib("eunit/include/eunit.hrl").

simple_test_() ->
    [?_assertEqual(2 + 2, 4)].
