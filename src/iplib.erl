
-module(iplib).

-include("defined.hrl").

-export([start/0, stop/0, reload/0, search/1]).

start() ->
    io:format("starting iplib"),
    Ret = application:start(?MODULE),
    io:format(" ~p.\n", [Ret]),
    gen_server:call(iplib_server, create).

stop()->
    application:stop(?MODULE).

search(Ip) when is_binary(Ip) ->
    search(binary_to_list(Ip));
search(Ip) when is_integer(Ip) ->
    case ets:select(iplib_ip_info, [{{'$1', '$2'}, [{is_integer, '$1'}, {is_integer, Ip}, {'>=', '$1', Ip}],['$2']}], 1) of
        {[Match], _C} ->
            if Match#iplib_ip_info.start > Ip ->
                null;
            true ->
                {ok, Match}
            end;
        '$end_of_table' ->
            null
    end;
search(Ip) ->
    case iplib_utils:ip2long(Ip) of
        {ok, Long} -> search(Long);
        {error, _} -> null
    end.

reload() ->
    gen_server:call(iplib_server, clean),
    gen_server:call(iplib_server, create).
