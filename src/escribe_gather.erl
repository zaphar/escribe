%% @doc Module holding the crawling functions
%% @author Jeremy (zaphar) Wall <jeremy@marzhillstudios.com>

-module(escribe_gather).
-author("Jeremy (zaphar) Wall <jeremy@marzhillstudios.com>").

-import(xmerl).
-import(xmerl_scan).
-import(xmerl_xpath).
-import(ibrowse).
-import(escribe_internal).
-import(escribe_evt).

%% processing libraries
-import(rss, [process_rss/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("escribedb.hrl").
%% @headerfile "../include/escribedb.hrl"

-export([crawl/1, crawl/0, crawl_proc/1]).
-export([get_http/1]).
-export([get_docs/1, process_doc/2, get_handler/2]).

%% @type exception() = {'EXIT', {undef, Stack}}
%% Stack = [{M,F,A}]
%% M = atom()
%% F = atom()
%% A = list()

%% @doc crawl feed sources from database
%% @spec crawl() -> any()
crawl() ->
  case escribe_db:db_list_src() of
    {atomic, L} when is_list(L) ->
      crawl(L);
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc crawl a list of feed sources or a single feed source from a record
%%  by spawning a process to do them
%% @spec crawl(Arg) -> any()
%%  Arg = [ source() ] | source()
crawl(L) when is_list(L) orelse is_record(L, sources) ->
    spawn(escribe_gather, crawl_proc, [L]).

%% @private
crawl_proc(L) when is_list(L) ->
  %% Aint list comprehensions grand?
  [crawl(X) || X <- L];
crawl_proc(L) when is_record(L, sources) ->
  Id   = L#sources.id,
  Type = L#sources.type, 
  case Type of
    http ->
      slurp(get_http(Id), Id);
    _   ->
      escribe_evt:error(string:concat("unhandled type: ", atom_to_list(Type))),
      ok
  end.

%% @doc get the docs from a feed automatically
%% using the correct protocol handler
%% @spec get_docs(D::source()) -> string()
%% @throws exception()
get_docs(D) when is_record(D, sources) ->
    Handler = get_handler(D#sources.type, protocol_handlers),
    apply(Handler, get_feed, []).

%% @doc process each doc in the feed
%% using the content_type handler for that feed
%% @spec process_doc(F::string(), Uri::string()) -> [document()]
%% @throws exception()
process_doc(F, Uri) when is_list(F) ->
    Type = escribe_util:interpret_doctype(F),
    Handler = get_handler(Type, content_handlers),
    apply(Handler, process_feed, [F, Uri]).

%% @doc get the handler module for a given handler type
%% @spec get_handler(Type::atom(), HandlerType::atom()) -> atom()
get_handler(Type, HandlerType) 
    when is_atom(Type) andalso is_atom(HandlerType) ->
    %% return our handler based off the escribe configuration
    {HandlerType, HandlerList} = escribe_conf:getkey(HandlerType),
    case HandlerList of
        undef ->
            default;
        List when is_list(List) ->
            case lists:keysearch(Type, 1, HandlerList) of
                {value, {Type, Module}} ->
                    Module;
                false ->
                    default
            end
    end.

%% @TODO
slurp(Str, Uri) when is_list(Str) andalso is_list(Uri) ->
    escribe_evt:debug(Str),
    escribe_evt:debug(string:concat("slurping: ", Uri)),
    rss_to_doclist(Str, Uri);
slurp({info, "aborted"}, Uri) when is_list(Uri) ->
    escribe_evt:info(string:concat("aborted: ", Uri)),
    ok.

%% @private
get_http(S) when is_list(S) ->
  Response = ibrowse:send_req(S, [], get), 
  case Response of
    {ok, "200", _Headers, Content} ->
      escribe_evt:info(string:concat("got response from: ", S)),
      Content;
    {ok, _, _, _} ->
      escribe_evt:info(string:concat("aborted response from: ", S)),
      {info, "aborted"};
    {error, Reason} ->
      escribe_evt:error(
        string:concat(string:concat("failed to retriev: ", S), Reason)
      ),
      {error, Reason}
  end.

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
