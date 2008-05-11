%% @author Jeremy Wall <jeremy@marzhillstudios.com>
%%
%% @doc escribe application callback module
%%
-module(escribe).
-behaviour(application).
-author("Jeremy Wall <jeremy@marzhillstudios.com>").
-vsn("0.01.0").

-export([start/0, start/2, stop/1]).

-import(escribe_server).

%% @doc start the application!! yay
start() ->
    application:start(escribe).

%% @doc start the application callback module
start(_Type, _Args) ->
    escribe_sup:start_link().

stop(_state) ->
    ok.
