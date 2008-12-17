-module(escribe_evt_test).
-author("Jeremy (zaphar) Wall <jeremy@marzhillstudios.com>").

-import(etap, [plan/1, ok/2, is/3]).
-import(etap_can, [can_ok/2, can_ok/3, loaded_ok/2]).
-import(etap_exception, [lives_ok/2]).
-import(escribe_util).

-export([start/0]).

start() ->
    plan(21),
    M = escribe_evt,
    loaded_ok(M, "loaded our escribe_evt module ok"),
    can_ok(M, start, 0),
    can_ok(M, start_link, 0),
    can_ok(M, handle_event, 2),
    can_ok(M, handle_call, 2),
    can_ok(M, handle_info, 2),
    can_ok(M, code_change, 3),
    can_ok(M, terminate, 2),
    escribe_evt:start(),
    escribe_conf:start_link(),
    is(escribe_evt:handle_info(blah, {}), {ok, {}},
        "handle_info returns state ok"),
    is(escribe_evt:handle_call(flush, [1,2,3]), {ok, flushed, []},
        "the flush call flushes all the log state"),
    is(escribe_evt:handle_call(view, [1,2,3]), {ok, [1,2,3], [1,2,3]},
        "the view call shows the log state"),
    is(escribe_evt:handle_call({view, 2}, [1,2,3]), {ok, [2,3], [1,2,3]},
        "the {view, 2} call shows the last 2 log entries"),
    is(escribe_evt:handle_call({view, 2}, [1]), {ok, [1], [1]},
        "the {view, 2} call shows the last 1 log entries when there is only one"),
    is(escribe_evt:handle_call(foo, [1,2,3]), {ok, {error, unhandled}, [1,2,3]},
        "unknown calls return an error"),
    {ok, List} = escribe_evt:handle_event({info, "foo"}, []),
    is(length(List), 1, "there is one log entry now"),
    [H | _T] = List,
    is(string:str(H, "INFO"), 1, "INFO is the first thing in the line"),
    ok(string:str(H, "foo") > 0, "foo is the in the line"),
    lives_ok(fun() -> escribe_evt:handle_call({warn, "foo"}, []) end,
        "warn messages work"),
    lives_ok(fun() -> escribe_evt:handle_call({debug, "foo"}, []) end,
        "debug messages work"),
    lives_ok(fun() -> escribe_evt:handle_call({error, "foo"}, []) end,
        "error messages work"),
    lives_ok(fun() -> escribe_evt:handle_call({fatal, "foo"}, []) end,
        "fatal messages work"),
    etap:end_tests().

