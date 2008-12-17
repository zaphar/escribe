%% @doc gen_event log handler
%% default log file is: escribe.log but you can override in the conf file
%% @author Jeremy Wall <jeremy@marzhillstudios.com>
-module(escribe_evt).
-behaviour(gen_event).
-author("Jeremy Wall <jeremy@marzhillstudios.com>").

-export([start/0, start_link/0, init/1]).
-export([handle_event/2, handle_call/2]).
-export([format_log_string/1]).
-export([handle_info/2, terminate/2]).
-export([code_change/3]).
-export([info/1, warn/1, debug/1, error/1, fatal/1]).
-export([flush/0, view/0, view/1]).

-include("escribe_log_util.hrl").
%% @headerfile "../include/escribe_log_util.hrl"

%% @doc start the logger local style
start() ->
    gen_event:start({local, escribe_logger}),
    gen_event:add_handler(escribe_logger, escribe_evt, []).

%% @doc start the logger supervised style
start_link() ->
    Return = gen_event:start_link({local, escribe_logger}),
    gen_event:add_handler(escribe_logger, escribe_evt, []),
    Return.

init(_Args) ->
    %% configurable logsize in memory?
    %% application:get_env(escribe, log_length)
    {ok, []}.

%% @doc log an info log item
%% @spec info(Msg::string()) -> ok
info(Msg) ->
    gen_event:notify(escribe_logger, {info, Msg}).

%% @doc log an warn log item
%% @spec warn(Msg::string()) -> ok
warn(Msg) ->
    gen_event:notify(escribe_logger, {warn, Msg}).

%% @doc log an debug log item
%% @spec debug(Msg::string()) -> ok
debug(Msg) ->
    gen_event:notify(escribe_logger, {debug, Msg}).

%% @doc log an error log item
%% @spec error(Msg::string()) -> ok
error(Msg) ->
    gen_event:notify(escribe_logger, {error, Msg}).

%% @doc log an fatal log item
%% @spec fatal(Msg::string()) -> ok
fatal(Msg) ->
    gen_event:notify(escribe_logger, {fatal, Msg}).

%% @doc log callbacks
%% @spec handle_event(E, State) -> {ok, State}
%%  E = logtype()
%%  State = []
handle_event({info, Msg}, State) ->
    Entry = format_log_string({"INFO", Msg}),
    log_it(Entry, State);
%% @clear 
%% warning log handler
handle_event({warn, Msg}, State) ->
    Entry = format_log_string({"WARN", Msg}),
    log_it(Entry, State);
%% debug information log handler
%% TODO(jwall): only when debug flags are set
handle_event({debug, Msg}, State) ->
    case os:getenv("DEBUG") of
        false ->
            {ok, State};
        _ ->
            Entry = format_log_string({"DEBUG", Msg}),
            log_it(Entry, State)
    end;
%% error log handler
handle_event({error, Msg}, State) ->
    Entry = format_log_string({"ERROR", Msg}),
    log_it(Entry, State);
%% fatality log handler
handle_event({fatal, Msg}, State) ->
    Entry = format_log_string({"FATAL", Msg}),
    log_it(Entry, State).

log_it(Entry, State) ->
    {log_file, Fn} = escribe_conf:getkey(log_file),
    case file:open(Fn, [append]) of
        {ok, IoDevice} ->
            io:format(IoDevice, "~p~n", [Entry]);
        {error, Reason} ->
            io:format("~p~n", [Reason]),
            io:format("~p~n", [Entry])
    end,
    {ok, lists:append(State, [Entry])}.

%% @TODO(jwall): document these methods
flush() ->
    gen_event:notify(escribe_logger, {debug, "flushing log file"}),
    gen_event:call(escribe_logger, escribe_evt, flush).
view() ->
    gen_event:notify(escribe_logger, {debug, "viewing entire log"}),
    gen_event:call(escribe_logger, escribe_evt, view).
view(N) ->
    gen_event:notify(escribe_logger, {debug, "viewing part of log"}),
    gen_event:call(escribe_logger, escribe_evt, {view, N}).

%% ask the logger to flush the log
handle_call(flush, _State) ->
    {ok, flushed, []};
%% ask to view the log
handle_call(view, State) ->
    {ok, State, State};
%% ask to view the log
handle_call({view, N}, State) ->
    case length(State) > N of
        true ->
            {LogView, _Rest}  = lists:split(N, lists:reverse(State)),
            {ok, lists:reverse(LogView), State};
        false ->
            {ok, State, State}
    end;
%% we at least want to know when an unknown request comes into the event logger
handle_call(Request, State) ->
    gen_event:notify(escribe_logger, 
        {debug, io_lib:format("unknown call: ~w", [Request])}),
    {ok, {error, unhandled}, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

