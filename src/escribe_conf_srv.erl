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
    ok.

init(_Args) ->
    {ok, {}}.

handle_call(_Src, _Call, _State) ->
    ok.

handle_info(_Call, _State) ->
    ok.

handle_cast(_Cast, _State) ->
    ok.

terminate(_Arg1, _Arg2) ->
    ok.

code_change(_Arg1, _Arg2, _Arg3) ->
    ok.

