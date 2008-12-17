-module(escribe_util_test).
-author("Jeremy (zaphar) Wall <jeremy@marzhillstudios.com>").

-import(etap, [plan/1, ok/2, is/3]).
-import(etap_can, [can_ok/2, can_ok/3, loaded_ok/2]).
-import(escribe_util).

-export([start/0]).

start() ->
    plan(13),
    loaded_ok(escribe_util, "loaded our escribe_util module ok"),
    can_ok(escribe_util, flatten_string_list, 1),
    can_ok(escribe_util, guid, 0),
    can_ok(escribe_util, char_list, 0),
    can_ok(escribe_util, rand_char, 1),
    can_ok(escribe_util, interpret_type, 1),
    can_ok(escribe_util, interpret_doctype, 1),
    is(escribe_util:interpret_type("http://"), http, 
        "successfully interpreted http protocol type"),
    is(escribe_util:interpret_type("ftp://"), ftp,
        "successfully interpreted ftp protocol type"),
    is(escribe_util:interpret_type("f://"),f,
        "successfully interpreted mythological f protocol type"),
    is(unknown, escribe_util:interpret_type("foo:blay"),
        "if it's not a uri string we return uknown"),
    is(length(escribe_util:char_list()),63,
        "charList returns the right number of characters"),
    is(length(escribe_util:guid()), 36,
        "guid returns the right number of characters"),
    etap:end_tests().
