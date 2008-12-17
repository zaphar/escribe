-module(escribe_release_test).
-author("Jeremy Wall <jeremy@marzhillstudios.com>").

-import(etap, [ok/2, is/3, plan/1]).
-import(etap_exception, [lives_ok/2]).
-import(etap_can, [can_ok/2, can_ok/3, loaded_ok/2, has_attrib/2,
                   is_attrib/3, is_behaviour/2]).

-export([start/0]).

start() ->
    plan(7),
    loaded_ok(escribe, "escribe application module is there"),
    loaded_ok(escribe_server, "escribe application module is there"),
    loaded_ok(escribe_sup, "escribe application module is there"),
    is_behaviour(escribe, application),
    is_behaviour(escribe_sup, supervisor),
    is_behaviour(escribe_server, gen_server),
    is_behaviour(escribe_evt, gen_event),
    etap:end_tests().
