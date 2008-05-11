%% @author Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>
%%
%% @doc Internal functions to the escribe application
%%
-module(escribe_internal).
-author("Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>").
-import(escribe_db).
-import(escribe_util, [interpret_type/1, interpret_doctype/1]).

-export([register_src/1, add_document/3, add_document/4, update_document/2, update_document/3]).

-include("escribedb.hrl").

%% @type msg_tuple() = ok_tuple() | err_tuple()
%%      ok_tuple()  = {ok, Msg}
%%      err_tuple() = {error, Msg}.
%%  a type of response.

%% @doc register a feed source
%%
%% returns {ok, "record added"} when successful
%%
%% returns {error,"Record already exists"} when the record already exists
%%
%% returns {error, Msg} for other errors
%%
%% @spec register_src(Source) -> msg_tuple()
%%  Source = string() | binary()
register_src(S) when is_list(S) ->
  T = interpret_type(S),
  escribe_db:db_add_src(#sources{id=S, type=T});
register_src(B) when is_binary(B) ->
  register_src(binary_to_list(B)).

%% @doc add a document to the database
%%
%% returns {ok, "Doc Added"} when successful
%%
%% returns {error, "Record already exists"} when the record already exists
%%
%% returns {error, "Source does not exist"} when the source does not exists
%%
%% {error, Msg} for other errors
%%
%% @spec add_document(Name::string(), Contents::string(), Src::string()) -> msg_tuple() 
add_document(Name, Contents, Src) when is_list(Name), is_list(Contents), is_list(Src) ->
  {Y, M, D} = erlang:date(),
  add_document(Name, Contents, Src, lists:concat([Y, "/", M, "/", D])).

%% @doc add a document to the database
%%
%% returns {ok, "Doc Added"} when successful
%%
%% returns {error, "Record already exists"} when the record already exists
%%
%% returns {error, "Source does not exist"} when the source does not exists
%%
%% {error, Msg} for other errors
%%
%% @spec add_document(Name::string(), Contents::string(), Src::string(), Date::string()) -> msg_tuple() 
add_document(Name, Contents, Src, Date) when is_list(Name), is_list(Contents), is_list(Src), is_list(Date) ->
  T = interpret_doctype(Contents),
  Doc = #documents{id=Name, type=T, date=Date, contents=Contents, src=Src},
  escribe_db:db_add_doc(Doc). 

%% @doc update a document in the database
%%
%% @spec update_document(Name::string(), Contents::string()) -> msg_tuple()
update_document(Name, Contents) when is_list(Name), is_list(Contents) ->
  {Y, M, D} = erlang:date(),
  update_document(Name, Contents, lists:concat([Y, "/", M, "/", D])).

%% @doc update a document in the database
%%
%% @spec update_document(Name::string(), Contents::string(), Date::string()) -> msg_tuple()
update_document(Name, Contents, Date) when is_list(Name), is_list(Contents), is_list(Date) ->
  case escribe_db:db_read_doc(Name) of
    {atomic, []} ->
        {error, "Doc does not exist"};
    {atomic, [Doc]} ->
        UDoc = #documents{
          id=Doc#documents.id,
          type=Doc#documents.type,
          contents=Contents,
          src=Doc#documents.src,
          date=Date
        },
        escribe_db:db_update_doc(UDoc);
    _ ->
        {error, "Unknown error"}
    end.
