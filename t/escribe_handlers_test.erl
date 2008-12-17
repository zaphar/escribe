-module(escribe_handlers_test).

-import(etap, [plan/1, ok/2, is/3]).
-import(etap_can, [loaded_ok/2, can_ok/2, can_ok/3]).
-import(etap_can, [has_attrib/2, is_attrib/3, is_behaviour/2]).

-export([start/0]).

start() ->
    plan(4),
    test_feed_handler_behaviour(),
    test_rss_handler(),
    etap:end_tests().

%% @doc 2 tests
test_rss_handler() ->
    loaded_ok(escribe_rss, "we loaded our escribe_rss module ok"),
    is_behaviour(escribe_rss, escribe_feed_handler_behaviour).

%% @doc 2 tests 
test_feed_handler_behaviour() ->
    loaded_ok(escribe_feed_handler_behaviour, "we loaded our behaviour module ok"),
    can_ok(escribe_feed_handler_behaviour, behaviour_info, 1).
