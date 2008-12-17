-module(escribe_feed_handler_behaviour).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{process_feed, 2}].
