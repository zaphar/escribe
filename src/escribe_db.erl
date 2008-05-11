%% @author Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>
%%
%% @doc mnesia interface functions for the escribe application
%%
-module(escribe_db).
-author("Jeremy Wall <jeremy@marzhillstudios.com>").

%% we import mnesia so that the namespace resolves correctly when using 
%% packages. (actually I don't think I need to do this now but I am anyway)
-import(mnesia).
-import(escribe_util, [interpret_type/1]).

-export([setup/0]).
-export([data_dir/0, has_tables/0]).
-export([sync_db/0, start_db/0, stop_db/0, info/0, info/1]).
-export([db_read_src/1, db_add_src/1, db_update_src/1]).
-export([db_list_src/0, db_list_src/1, db_list_src/2]).
-export([db_read_doc/1, db_add_doc/1, db_update_doc/1]).
-export([db_list_doc/0, db_list_doc/1, db_list_doc/2]).

-include("escribedb.hrl").
%% @headerfile "../include/escribedb.hrl"

%% @type msg_tuple() = ok_tuple() | err_tuple()
%%      ok_tuple()  = {ok, Msg}
%%      err_tuple() = {error, Msg}.
%%  a type of response.
%%


%% @doc returns info about the mnesia database
%% @spec info() -> Info
%%  Info = [InfoTuple]
%%  InfoTuple = {Name, Value}
%%  Name = string()
%%  Value = term()
info() ->
    mnesia:system_info(all).

%% @doc returns info about the mnesia database
%% @spec info(T::atom()) -> term()
info(Type) ->
    mnesia:system_info(Type).

%% @doc returns the data director for the running mnesia database
%% @spec data_dir() -> string()
data_dir() ->
    mnesia:system_info(directory).

%% @doc returns true when the tables have been setup in mnesia
%% @spec has_tables() -> bool()
has_tables() ->
    case info(tables) of
        [schema] ->
            false;
        _ ->
            true
    end.

%% @doc setup the mnesia database
%%
%% @spec setup() -> ok
setup() ->
    case has_tables() of
        false ->
            create_tables(),
            sync_db();
        true ->
            ok
    end.
          
%% @doc create the mnesia tables
%%
%% @spec create_tables() -> Return
%% Return = {atomic, ok} | {error, Reason}
create_tables() ->
   mnesia:create_table(
                       sources, [
                                 {disc_copies, [node()]},
                                 {attributes, record_info(fields, sources)}
                                ]
                      ),
   mnesia:create_table(
                       documents, [
                                 {disc_copies, [node()]},
                                 {attributes, record_info(fields, documents)}
                                ]
                      ),
   mnesia:create_table(
                       config, [
                                 {disc_copies, [node()]},
                                 {attributes, record_info(fields, config)}
                                ]
                      ).

%% @doc ensure the database is ready for us
%%
%% If it returns {warning, list()} then the tables in the list are
%% still not safe to run against, but the timeout expired.
%% Otherwise, an {ok, started} response means we are all set to go.
%% 
%% @spec sync_db() -> Return
%% Return = {ok, started} | {warning, RemainingTabs}
sync_db() ->
  case mnesia:wait_for_tables([sources, documents, config], 300) of
    {timeout, RemainingTabs} ->
      {warning, RemainingTabs};
    ok ->
      {ok, started}
  end.

%% @doc start the mnesia database
%% 
%% returns are the same as for sync_db() 
%% 
%% @spec start_db() -> Return
%% Return = {ok, started} | {warning, RemainingTabs} 
start_db() ->
  mnesia:start(),
  sync_db().

%% @doc stops the mnesia database
%% 
%% @spec stop_db() -> stopped
stop_db() ->
  mnesia:stop().

%% @doc read the src record from the database
%% 
%% @spec db_read_src(Id::string()) -> db_response() 
db_read_src(Id) ->
  db_read_it(sources, Id).

%% @doc read the document record from the database
%% 
%% @spec db_read_doc(Id::string()) -> db_response() 
db_read_doc(Id) when is_list(Id) ->
  db_read_it(documents, Id).

%% @doc add the src record to the database
%%  
%% returns {ok, "Source added"} on success
%%  
%% returns {error, Reason} for errors 
%% 
%% @spec db_add_src(Src) -> db_response()
%%  Src = string() | source()
db_add_src(Url) when is_list(Url) ->
  db_add_src(#sources{id=Url, type=interpret_type(Url)});
db_add_src(Src) when is_record(Src, sources) ->
  F = db_add_it(Src, fun() -> db_read_src(Src#sources.id) end),
  process_transaction(F, "Source added").

%% @doc add a document to the database
%%
%% returns {ok, "Doc added"} on success.
%%  
%% returns {error, Reason} on error
%%  
%% @spec db_add_doc(Doc) -> db_response()
%%  Doc = document()
db_add_doc(Doc) when is_record(Doc, documents) ->
  %% we need to make sure the source exists before adding this
  %% so we wrap the whole thing in a transaction to ensure it
  Action = fun() -> 
      S = db_read_src(Doc#documents.src),
      case S of
        {atomic, []} ->
          mnesia:abort("Source does not exist");
        {atomic, _} ->
          F = db_add_it(Doc, fun() -> db_read_doc(Doc#documents.id) end),
          F();
        {error, _Msg} ->
          mnesia:abort(_Msg)
      end
    end,
  process_transaction(Action, "Doc added").

%% @doc update a source
%%
%% @spec db_update_src(Src) -> db_response()
%%  Src = source()
db_update_src(Src) when is_record(Src, sources) ->
  Action = db_update_it(Src, fun() -> db_read_src(Src#sources.id) end),
  process_transaction(Action, "Source updated").

%% @doc update a doc
%%
%% @spec db_update_doc(Doc) -> db_response()
%%  Doc = document()
db_update_doc(Doc) when is_record(Doc, documents) ->
  Action = db_update_it(Doc, fun() -> db_read_doc(Doc#documents.id) end),
  process_transaction(Action, "Doc updated").

%% @private
%% a function that returns a function. I love this stuff :-)
db_update_it(R, F) when is_function(F) ->
  fun() ->
    case F() of 
      {atomic, []} ->
        mnesia:abort("Record does not exist");
      {atomic, _} ->
        %% should I support constraints callbacks here?
        mnesia:write(R)
    end
  end.

%% @clear
%% queries

%% @doc list all sources with no sort specified
%% uses default sort type of id.
%% @spec db_list_src() -> db_response()
db_list_src() ->
  db_list_src({type, '_'}).

%% @doc list all sources of particuler type.
%% uses default sort type of id.
%% @spec db_list_src(Restrict) -> db_response()
%%  Restrict = {type, Type}
%%  Type = string()
db_list_src({type, Type}) when is_atom(Type) ->
  db_list_src({type, Type}, {sort, id}).

%% @doc list sources sorted by id or a user defined function
%% @spec db_list_src(A::Restrict, B::Sort) -> db_response()
%%  Restrict = {type, string()}
%%  Sort = {sort, id} | function()
db_list_src({type, Type}, {sort, id}) when is_atom(Type) ->
  F = fun(E1, E2) -> E1#sources.id =< E2#sources.id end,
  db_list_src({type, Type}, F);
db_list_src({type, Type}, F) when is_function(F) ->
  case mnesia:transaction(fun() -> mnesia:match_object(#sources{_='_', type=Type}) end) of
    {atomic, []} ->
      {atomic, []};
    {atomic, R} when is_list(R) ->
      {atomic, lists:sort(F, R)};
    _ ->
      {error, "unexpected error"}
  end.

%% @doc list all docs with no sort specified
%% @spec db_list_doc() -> db_response()
db_list_doc() ->
  db_list_doc({type, '_'}).

%% @doc list docs of certain type with no sort
%% @spec db_list_doc(A::Restrict) -> db_response()
%%  Restrict = {type, string()}
db_list_doc({type, Type}) when is_atom(Type) ->
  db_list_doc({type, Type}, {sort, id}).

%% @doc list sources sorted by id or a user defined function
%% @spec db_list_doc(A::Restrict, B::Sort) -> db_response()
%%  Restrict = {type, string()}
%%  Sort = {sort, id} | function()
db_list_doc({type, Type}, {sort, id}) when is_atom(Type) ->
  F = fun(E1, E2) -> E1#documents.date =< E2#documents.date end,
  db_list_doc({type, Type}, F);
db_list_doc({type, Type}, F) when is_function(F) ->
  case mnesia:transaction(fun() -> mnesia:match_object(#documents{_='_', type=Type}) end) of
    {atomic, []} ->
      {atomic, []};
    {atomic, R} when is_list(R) ->
      {atomic, lists:sort(F, R)};
    _ ->
      {error, "unexpected error"}
  end.

%% helper functions
%% @private
process_transaction(F, Msg) when is_function(F) ->
  case mnesia:transaction(F) of
    {aborted, _msg} ->
      {error, _msg};
    {atomic, _} ->
      % success!!
      {ok, Msg}
  end.

%% @private
db_add_it(R, F) ->
  fun() ->
    case F() of 
      {atomic, []} ->
        % we are adding
        mnesia:write(R);
      {atomic, _Returned} ->
        % Error!!! record already exists
        mnesia:abort("Record already exists")
    end
  end.

%% @private
db_read_it(Tab, Id) ->
  ReadIt = fun() ->
    mnesia:read({Tab, Id})
  end,
  mnesia:transaction(ReadIt).

