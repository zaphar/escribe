%% @type source() = {id, type}.
%%
%% @type document() = {id, type, date, contents, src}.
%%
%% @type config() = {name, value}.
%%
%% @type db_response() = {atomic, list()} | msg_tuple().
%%  a type of response from the database calls.
%%      

-record(sources, {id,
                  type}).

-record(documents, {id,
                    type,
                    date,
                    contents,
                    src}).

-record(config, {name,
                 value}).

