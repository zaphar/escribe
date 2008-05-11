-module(escribe_db_test).
-author("Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>").

-import(etap).
-import(etap_can, [can_ok/2, can_ok/3, loaded_ok/2]).

-export([start/0]).

-include("escribedb.hrl").

start() ->
  etap:plan(41),
  etap_can:loaded_ok(escribe_db, "escribe_db module loaded ok"),
  can_ok(escribe_db, setup, 0),
  can_ok(escribe_db, sync_db, 0),
  can_ok(escribe_db, db_read_src, 1),
  can_ok(escribe_db, db_read_doc, 1),
  can_ok(escribe_db, db_add_src, 1),
  can_ok(escribe_db, db_add_doc, 1),
  can_ok(escribe_db, db_update_src, 1),
  can_ok(escribe_db, db_update_doc, 1),
  can_ok(escribe_db, db_list_src, 0),
  can_ok(escribe_db, db_list_doc, 0),
  can_ok(escribe_db, db_list_src, 1),
  can_ok(escribe_db, db_list_src, 2),
  can_ok(escribe_db, db_list_doc, 1),
  can_ok(escribe_db, db_list_doc, 2),
  mnesia:create_schema([node()]),
  escribe_db:start_db(),
  escribe_db:setup(),
  escribe_db:sync_db(),
  etap:is(escribe_db:db_read_src("http://foo/"), {atomic, []}, "http://foo/ has not been added to the sources table yet"),
  etap:is(escribe_db:db_add_src(#sources{id="http://foo/", type=http}), {ok, "Source added"}, "http://foo/ was added successfully"),
  etap:is(escribe_db:db_add_src(#sources{id="http://foo/", type=http}), {error, "Record already exists"}, "http://foo/ was not added successfully Record already exists"),
  etap:is(escribe_db:db_read_src("http://foo/"), {atomic,[#sources{id="http://foo/", type=http}]}, "http://foo/ was retrieved successfull"),
  etap:is(escribe_db:db_update_src(#sources{id="http://foo1/", type=ftp}), {error, "Record does not exist"}, "http://foo1/ update failed with no record"),
  etap:is(escribe_db:db_update_src(#sources{id="http://foo/", type=ftp}), {ok, "Source updated"}, "http://foo/ was added successfully"),
  etap:is(escribe_db:db_read_src("http://foo/"), {atomic,[#sources{id="http://foo/", type=ftp}]}, "http://foo/ was retrieved successfull"),
  etap:is(escribe_db:db_list_src(), {atomic,[#sources{id="http://foo/", type=ftp}]}, "list of records was retrieved"),
  etap:is(escribe_db:db_list_src({type, ftp}), {atomic,[#sources{id="http://foo/", type=ftp}]}, "list of ftp records was retrieved"),
  etap:is(escribe_db:db_list_src({type, http}), {atomic,[]}, "no list of http records was retrieved"),
  etap:is(escribe_db:db_add_src(#sources{id="http://foo1/", type=http}), {ok, "Source added"}, "http://foo1/ was added successfully"),
  etap:is(escribe_db:db_add_src(#sources{id="http://foo2/", type=http}), {ok, "Source added"}, "http://foo2/ was added successfully"),
  etap:is(escribe_db:db_list_src({type, http}), {atomic,[#sources{id="http://foo1/", type=http},#sources{id="http://foo2/", type=http}]}, "list of http records was retrieved sorted correctly"),
  etap:is(escribe_db:db_read_doc("Foo 1"), {atomic, []}, "document Foo 1 has not been added to the document table yet"),
  etap:is(escribe_db:db_list_doc(), {atomic,[]}, "list of records was retrieved"),
  etap:is(escribe_db:db_add_doc(#documents{id="Foo 1", type=html, date="1/01/2008", src="http://foo/"}), {ok, "Doc added"}, "Foo 1 was added successfully"),
  etap:is(escribe_db:db_list_doc(), {atomic,[#documents{id="Foo 1", type=html, date="1/01/2008", src="http://foo/"}]}, "list of records was retrieved"),
  etap:is(escribe_db:db_read_doc("Foo 1"), {atomic, [#documents{id="Foo 1", type=html, date="1/01/2008", src="http://foo/"}]}, "document Foo 1 was added to the document table"),
  etap:is(escribe_db:db_add_doc(#documents{id="Bar 1", type=html, date="1/01/2008", src="http://bar/"}), {error, "Source does not exist"}, "Bar 1 errors with non existent source"),
  etap:is(escribe_db:db_add_doc(#documents{id="Foo 2", type=html, date="1/02/2008", src="http://foo/"}), {ok, "Doc added"}, "Foo 2 was added successfully"),
  etap:is(escribe_db:db_update_doc(#documents{id="Foo 3", type=html, date="1/02/2008", contents="foo is a word oft used in tests", src="http://foo/"}), {error, "Record does not exist"}, "Foo 3 update failed with no record"),
  etap:is(escribe_db:db_update_doc(#documents{id="Foo 2", type=html, date="1/02/2008", contents="foo is a word oft used in tests", src="http://foo/"}), {ok, "Doc updated"}, "Foo 2 was added successfully"),
  etap:is(escribe_db:db_read_doc("Foo 2"), {atomic, [#documents{id="Foo 2", type=html, date="1/02/2008", contents="foo is a word oft used in tests", src="http://foo/"}]}, "document Foo 2 contents match updated record"),
  etap:is(escribe_db:db_list_doc(), {atomic,[#documents{id="Foo 1", type=html, date="1/01/2008", src="http://foo/"},#documents{id="Foo 2", type=html, contents="foo is a word oft used in tests", date="1/02/2008", src="http://foo/"}]}, "list of records was retrieved"),
  etap:is(escribe_db:db_list_doc({type, html}), {atomic,[#documents{id="Foo 1", type=html, date="1/01/2008", src="http://foo/"},#documents{id="Foo 2", type=html, contents="foo is a word oft used in tests", date="1/02/2008", src="http://foo/"}]}, "list of html records was retrieved"),
  etap:is(escribe_db:db_list_doc({type, rss}), {atomic, []}, "there were no rss records retrieved"),
  escribe_db:stop_db(),
  etap:end_tests().

