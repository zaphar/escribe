format_log_string({Type, Msg}) ->
    {{Y, M, D}, {H, Mn, S}} = calendar:local_time(),
    Time = io_lib:format("~B/~B/~B ~B:~B:~B", [Y, M, D, H, Mn, S]),
    lists:flatten(io_lib:format("~s|~s|~s", [Type, Time, Msg])).

%% @type logtype() = {info, Msg} | {warn, Msg} | {debug, Msg} | {error, Msg} | {fatal, Msg}
%%  Msg = string().
%%  various allowed logging requests
