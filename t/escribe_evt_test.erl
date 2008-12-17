-module(escribe_evt_test).
-author("Jeremy (zaphar) Wall <jeremy@marzhillstudios.com>").

-import(etap, [plan/1, ok/2, is/3]).
-import(etap_can, [can_ok/2, can_ok/3, loaded_ok/2]).
-import(escribe_util).

-export([start/0]).

start() ->
    plan(4),
    M = escribe_evt,
    loaded_ok(M, "loaded our escribe_evt module ok"),
    can_ok(M, handle_event, 2),
    can_ok(M, handle_call, 2),
    can_ok(M, handle_info, 2),
    %NewKey = {baz, [blah]},
    %State = [{foo, [bar]}, {bar, [foo]}],
    %State2 = [{foo, [bar]}, {bar, [foo]}, NewKey],
    %is(escribe_conf: handle_cast({putmy,NewKey}, State),
    %    {noreply, State2},
    %    "handle cast does put the new tuple in"),
    etap:end_tests().

