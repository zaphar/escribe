%% @doc Module holding the crawling functions
%% @author Jeremy (zaphar) Wall <jeremy@marzhillstudios.com>
-module(escribe_rss).
-author("Jeremy (zaphar) Wall <jeremy@marzhillstudios.com>").
-behaviour(escribe_feed_handler_behaviour).

-import(escribe_internal).
-import(escribe_evt).

%% processing libraries
-import(rss, [process_rss/1]).

-include("escribedb.hrl").
%% @headerfile "../include/escribedb.hrl"

-export([process_feed/2]).

process_feed(Rss, Uri) ->
    rss_to_doclist(Rss, Uri).

%% @private
rss_to_doclist(Rss, Uri) when is_list(Rss) ->
  escribe_evt:info(string:concat("processing : ", Uri)),
  Items = escribe_util:flatten_string_list([ C || C <- process_rss(Rss) ]),
  [rss_item_to_doc(D, Uri) || D <- Items].

%% @private
%% @type resultlist() = list(Results)
%%  Results = {info, Msg} | ok 
rss_item_to_doc(I, Src) when is_tuple(I) ->
  %% I probably need to handle the case of document already exists here?
  Name = element(2, I),
  Contents = element(5, I),
  case escribe_internal:add_document(Name, Contents, Src) of
    {error, "Record already exists"} ->
      %% update here?
      case escribe_internal:update_document(Name, Contents) of
        {ok, "Doc updated"} ->
          escribe_evt:info(string:concat("Updated Document: ", Name)),
          {info, "updating doc"};
        {error, Msg} ->
          escribe_evt:error(string:concat("unknown error: ", Msg)),
          %% weird error case uhoh!!!
          throw(lists:concat("unknown update error encountered: ", Msg))
      end;
    {ok, "Doc added"} ->
      ok;
    {error, Msg} ->
      escribe_evt:error(string:concat("unknown error encountered: ", Msg)),
      throw(lists:concat("unknown doc added error encountered", Msg))
  end;
rss_item_to_doc([], Src) when is_list(Src) ->
    escribe_evt:info("No items in feed"),
    ok.
