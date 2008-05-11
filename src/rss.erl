-module(rss).
-author("Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>").

-import(xmerl).
-import(xmerl_scan).
-import(xmerl_xpath).

-include_lib("xmerl/include/xmerl.hrl").

-export([process_rss/1, parse_rss_channels/1]).

%% @type channel() = #channel{uri=Uri, title=Title, desc=Desc, items=Items}
%%  Items = [ rssitem() ]
%%  Uri = string()
%%  Title = string()
%%  Desc = string().

%% @type rssitem() = #rssitem{uri=Uri, title=Title, desc=Desc, content=Content, dt=Dt}
%%  Uri = string()
%%  Title = string()
%%  Desc = string()
%%  Content = string()
%%  Dt = string().

-record(channel, {uri, title, desc, items}).
-record(rssitem, {uri, title, desc, content, dt}).

%% @doc parse out an rss feed
%% @spec process_rss(S::string()) -> [I]
%%  I = rssitem()
process_rss(S) when is_list(S) ->
  Doc = xml_doc(S),
  lists:flatten([ parse_rss_item(I) || I <- xmerl_xpath:string("//item", Doc) ]).

%% @doc parse out an rss feeds channels
%% @spec parse_rss_channels(Rss) -> [C]
%%  Rss = string() | #xmlElement{}
%%  C = channel()
parse_rss_channels(L) when is_list(L) ->
  [parse_rss_channels(C) || C <- L]; 
parse_rss_channels(C) when is_record(C, xmlElement) ->
  Title  = xpath_text_content("//title", C),
  Desc   = xpath_text_content("//description", C),
  Uri   = xpath_text_content("//link", C),
  Itemlist = xmerl_xpath:string("//item", C),
  #channel{ uri=Uri, title=Title, desc=Desc, items=[parse_rss_item(I) || I <- Itemlist] }.

%% @private
parse_rss_item(I) when is_record(I, xmlElement) ->
  Link = xpath_text_content("//link", I), 
  Title = xpath_text_content("//title", I), 
  Desc = xpath_text_content("//description", I), 
  Content = xpath_text_content("//content:encoded", I), 
  Date = xpath_text_content("//pubDate", I),
  #rssitem{uri=Link, title=Title, desc=Desc, content=Content, dt=Date}.

%% @private
xpath_first_item(Str, R) when is_record(R, xmlElement) ->
  case xmerl_xpath:string(Str, R) of
  [H |_] ->
    H;
  [] ->
    #xmlElement{}
  end.

%% @private
xpath_text_content(Str, R) when is_record(R, xmlElement) ->
  Text = xpath_content(Str, R),
  lists:flatten([T#xmlText.value || T <- Text]).

%% @private
xpath_content(Str, R) when is_record(R, xmlElement) ->
  Node  = xpath_first_item(Str, R),
  Node#xmlElement.content.

%% @private
xml_doc(S) when is_list(S) ->
  {Doc, _} = xmerl_scan:string(S),
  Doc.

