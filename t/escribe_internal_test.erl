-module(escribe_internal_test).
-author("Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>").

-import(etap).
-import(etap_can).

-export([start/0]).

-include("escribedb.hrl").

start() ->
  etap:plan(14),
  etap_can:loaded_ok(escribe_db, "escribe_db module loaded ok"),
  etap_can:loaded_ok(escribe_internal, "escribe_internal module loaded ok"),
  mnesia:create_schema([node()]),
  escribe_db:start_db(),
  escribe_db:setup(),
  escribe_db:sync_db(),
  etap_can:can_ok(escribe_internal, register_src, 1),
  Sone = "http://escribe.internal/test/src1",
  etap:is(escribe_internal:register_src(Sone), {ok, "Source added"}, "added a source properly"),
  etap:is(escribe_internal:register_src(<<"http://escribe.internal/test/src2">>), {ok, "Source added"}, "added a binary string source properly"),
  etap_can:can_ok(escribe_internal, add_document, 3),
  etap_can:can_ok(escribe_internal, add_document, 4),
  FooName = "foo doc",
  etap:is(escribe_internal:add_document(FooName, "foo 1 foo 2 foo 3", Sone), {ok, "Doc added"}, "added a doc properly"),
  etap:is(escribe_internal:add_document("bar doc", "bar 1 bar 2 bar 3", Sone, "2/12/2008"), {ok, "Doc added"}, "added a doc properly"),
  etap:is(escribe_internal:add_document(FooName, "foo 1 foo 2 foo 3", Sone), {error, "Record already exists"}, "Failed to add a doc"),
  etap_can:can_ok(escribe_internal, update_document, 2),
  etap_can:can_ok(escribe_internal, update_document, 3),
  etap:is(escribe_internal:update_document(FooName, "foo 4 foo 5 foo 6"), {ok, "Doc updated"}, "updated doc ok"),
  etap:is(escribe_internal:update_document("bar", "foo 4 foo 5 foo 6"), {error, "Doc does not exist"}, "failed to update doc that does not exist"),
  escribe_db:stop_db(),
  etap:end_tests().
