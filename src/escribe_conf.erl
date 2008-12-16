%% @author Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>
%% @doc gen_server callback module for application configuration stuff
-module(escribe_conf).
-author("Jeremy Wall <jeremy@marzhillstudios.com").
-behaviour(gen_server).

-import(escribe_util).
-import(escribe_db).

-export([start_link/0, init/1]).
-export([handle_call/3, handle_info/2, handle_cast/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([getkey/1, putkey/1]).

start_link() ->
    gen_server:start_link({local, escribe_conf}, escribe_conf, [], []).

init(_Args) ->
    case file:script("escribe.conf") of
        {ok, Value} when is_list(Value) ->
            {ok, lists:keymerge(2, Value, default())};
        {ok, _BadValue} ->
            {ok, default()};
        {error, _Reason} ->
            {ok, default()}
    end.

default() ->
    [
     {content_handlers,  [{rss, escribe_rss}]},
     {protocol_handlers, [{http, escribe_http}]}
    ].

%% @doc get configuration key
%% @spec getkey(Key::atom()) -> tuple()
getkey(Key) ->
    gen_server:call(escribe_conf, {getmy, Key}).

%% @doc store configuration key
%% @spec putkey(Tuple::tuple()) -> ok
putkey(Tuple) ->
    gen_server:call(escribe_conf, {putmy, Tuple}).

%% @doc handle the config server requests
%% @spec handle_call(Call, Src, State) -> {reply, tuple(), State}
%%  Call = {getmy, atom()} | {putmy, tuple()}
%%  State = [tuple()]
handle_call({getmy, Key}, _Src, State) when is_atom(Key) ->
    case lists:keysearch(Key, 1, State) of
        {value, Tuple} ->
            {reply, Tuple, State};
        false ->
            {reply, {}, State}
    end;
handle_call({putmy, Tuple}, _Src, State) when is_tuple(Tuple) ->
    {reply, ok, update_state(Tuple, State)}.

%% @doc handle the request to set a config item
%% @spec handle_cast(Call, State) -> {noreply, State}
%%  Call = {putmy, tuple()}
%%  State = [tuple()]
handle_cast({putmy, Tuple}, State) when is_tuple(Tuple) ->
    {noreply, update_state(Tuple, State)}.

%% @doc unused
handle_info(_Call, _State) ->
    ok.

%% @private
update_state(Tuple, State) ->
    lists:keystore(element(1, Tuple), 1, State, Tuple).

%% @TODO
terminate(_Arg1, _Arg2) ->
    ok.

%% @TODO
code_change(_Arg1, _Arg2, _Arg3) ->
    ok.

