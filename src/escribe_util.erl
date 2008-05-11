%% @doc utility functions for escribe
%%
%% @author Jeremy Wall <jeremy@marzhillstudios.com>
-module(escribe_util).
-author("Jeremy (Zaphar) Wall <jeremy@marzhillstudios.com").

-import(lists).
-import(string).
-export([interpret_type/1, interpret_doctype/1]).
-export([flatten_string_list/1]).
-export([guid/0, char_list/0, rand_char/1]).

%% @doc interpret the type of the feed
%%
%% @spec interpret_type(S) -> string()
%%  S = string()
interpret_type(S) when is_list(S) ->
  case string:str(S, "://") of
    0 ->
      %% whoah this aint no URI string
      unknown;
    1 -> 
      [H |_] = S,
      H;
    N when is_number(N) ->
      list_to_atom(string:substr(S, 1, N-1))
  end.

%% @doc interpret doctype of a document unused at the moment.
%% @spec interpret_doctype(S) -> unused
%%  S = string()
interpret_doctype(S) when is_list(S) ->
  unused.

%% @doc shamelessly ripped from the lists module but this one respects strings
%% @spec flatten_string_list(L) -> list()
%%  L = list()
flatten_string_list(List) when is_list(List) ->
    flatten_string_list(List, []).

flatten_string_list([H|T], Tail) when is_list(H) ->
    case io_lib:char_list(H) of
      false ->
        flatten_string_list(H, flatten_string_list(T, Tail));
      true ->
        [H | flatten_string_list(T, Tail)]
    end;
flatten_string_list([H|T], Tail) ->
    [H|flatten_string_list(T, Tail)];
flatten_string_list([], Tail) ->
    Tail.

%% @doc list representing all the characters A-Za-z0-9
%% @spec char_list() -> [char()]
char_list() ->
    [ $A | char_list(char_list($A)) ++ [$0,$1,$2,$3,$4,$5,$6,$7,$8,$9]].

char_list(I) when is_integer(I) ->
    case I == $z of
        true ->
            [I];
        false ->
            [I | char_list(I + 1)]
    end;
char_list(L) when is_list(L) ->
    F = fun(I) ->
        (I < $[) orelse (I > $`) end,
    lists:filter(F, L).

%% @doc random string of characters generator
%% @spec rand_char(N) -> [char()]
%%  N = integer()
rand_char(I) when is_integer(I) ->
    [rand_char() | rand_char({I, 1})];
rand_char({I, N}) ->
    case I == N of
        true ->
            [];
        false ->
            [rand_char() | rand_char({I, N+1})]
    end.

%% @private
rand_char() ->
    L = char_list(),
    C = random:uniform(length(L)),
    lists:nth(C, L).

%% @doc GUID generator for type 4 GUIDS
%% @spec guid() -> [char()]
guid() ->
    string:join([rand_char(8),
                      rand_char(4),
                      rand_char(4),
                      rand_char(4),
                      rand_char(12)], "-").
