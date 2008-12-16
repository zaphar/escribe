-module(escribe_sup).
-behaviour(supervisor).

-author("Jeremy Wall <jeremy@marzhillstudios.com>").

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, escribe_supervisor}, escribe_sup, []).

init(_Args) ->
    {ok, {{one_for_one, 1, 60},
          [
           %% child specifications
           %% start our event logger
           {escribe_conf, {escribe_conf, start_link, []},
            permanent, brutal_kill, worker, [escribe_conf]},
           %% start our event logger
           {escribe_evt, {escribe_evt, start_link, []},
            permanent, brutal_kill, worker, [escribe_evt]},
           %% start the escribe gen_server process
           {escribe_server, {escribe_server, start_link, []},
            permanent, brutal_kill, worker, [escribe_server]}
          ]}}.

