%% @author Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com>
%% @doc gen_server callback module for application configuration stuff
-module(escribe_conf_srv).
-author("Jeremy Wall <jeremy@marzhillstudios.com").
-behaviour(gen_server).

-import(escribe_util).
-import(escribe_db).

-export([start_link/0, start/0, init/1]).
-export([handle_call/3, handle_info/2, handle_cast/2]).
-export([terminate/2]).
-export([code_change/3]).

start() ->
    ok.

start_link() ->
    gen_server:start_link({local, escribe_conf}, escribe_conf_srv, [], []).

init(_Args) ->
    %% first we need to load from the database
    %% then we need to load from the file
    {ok, []}.

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

