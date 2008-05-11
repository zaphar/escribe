%% @author Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>
%%
%% @doc gen_server callback module for the escribe app
%%
-module(escribe_server).
-behaviour(gen_server).
-author("Jeremy Wall <jeremy@marzhillstudios.com>").

-import(escribe_db).
-import(escribe_gather).
-import(escribe_internal).

-export([start_link/0, init/1]).
-export([register_src/1]).
-export([crawl/1, crawl/0]).
-export([handle_call/3, handle_cast/2]).
-export([code_change/3]).
-export([handle_info/2, terminate/2]).

%% @doc start our escribe server
start_link() ->
    gen_server:start_link({local, escribe_srv}, escribe_server, [], []).

%% @doc init the server we don't really have any state to track right now. 
%% Maybe later on.
init(_Args) ->
    escribe_db:start_db(),
    {ok, {ok, []}}.

%% @doc make a register_src call to the server
register_src(Src) when is_list(Src) ->
    gen_server:call(escribe_srv, {register_src, Src}).

%% @doc make a crawl all feeds call to the server
crawl() ->
    gen_server:call(escribe_srv, crawl).

%% @doc make a crawl feed call to the server
crawl(Src) ->
    gen_server:call(escribe_srv, {crawl, Src}).

%% @spec handle_call(Call, From, State) -> {reply, Response, State}
%%  Call = crawl | {crawl, Src} | {register_src, Src}
%%  Src = string()
%%  From = pid()
%%  State = tuple()
handle_call(crawl, _From, State) ->
    {reply, escribe_gather:crawl(), State};
handle_call({crawl, Src}, _From, State) ->
    {reply, escribe_gather:crawl(Src), State};
handle_call({register_src, Src}, _From, State) when is_list(Src) ->
    Response = escribe_internal:register_src(Src),
    {reply, Response, State}.

%% @doc no casts to handle
handle_cast(_Req, State) ->
    {noreply, State}.

%% TODO(jwall): how do I do this?
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% TODO(jwall): do I have any of these?
handle_info(_Info, State) ->
    {noreply, State}.

%% we don't have any terminations cleanup to do
terminate(_Reason, _State) -> 
    ok.

