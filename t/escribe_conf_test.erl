-module(escribe_conf_test).
-author("Jeremy (zaphar) Wall <jeremy@marzhillstudios.com>").

-import(etap, [plan/1, ok/2, is/3]).
-import(etap_can, [can_ok/2, can_ok/3, loaded_ok/2]).
-import(escribe_util).

-export([start/0]).

start() ->
    plan(12),
    loaded_ok(escribe_conf, "loaded our escribe_conf module ok"),
    can_ok(escribe_conf, handle_call, 3),
    can_ok(escribe_conf, handle_cast, 2),
    can_ok(escribe_conf, handle_info, 2),
    can_ok(escribe_conf, terminate, 2),
    can_ok(escribe_conf, code_change, 3),
    NewKey = {baz, [blah]},
    State = [{foo, [bar]}, {bar, [foo]}],
    State2 = [{foo, [bar]}, {bar, [foo]}, NewKey],
    is(escribe_conf:handle_call({getmy, foo}, 1, State),
        {reply, {foo, [bar]}, State}, 
        "handle call does return foo key and state"),
    is(escribe_conf: handle_call({putmy,NewKey}, 1, State),
        {reply, ok, State2},
        "handle call does put the new tuple in"),
    is(escribe_conf: handle_cast({putmy,NewKey}, State),
        {noreply, State2},
        "handle cast does put the new tuple in"),
    is(escribe_conf:handle_info(1,2), ok,
        "handle_info returns ok"),
    is(escribe_conf:terminate(1,2), ok,
        "terminate returns ok"),
    is(escribe_conf:code_change(1,2,3), ok,
        "code_change returns ok"),
    etap:end_tests().

