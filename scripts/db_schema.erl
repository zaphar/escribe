#!/usr/bin/env escript
%% -*- mode: erlang -*-
%%! -pa ebin
-export([main/1]).

-import(escribe_db).

main(_) ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  escribe_db:setup().

