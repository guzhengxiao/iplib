
-module(iplib_server).

-behaviour(gen_server).

-include("defined.hrl").

-record(state, {info_tab}).  

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

init([]) ->
    {ok, #state{}}.
    %io:format("~p ~n", [ets:select(TableId, [{{'$1', '$2'}, [{'>=', '$1',16777472}], ['$2']}], 1)]), 

handle_call(create, _From, State) ->
    io:format("creating ip info ~n"),
    Tab = create_ets(),
    io:format("~p created successful~n", [ets:info(Tab, size)]),
    {reply, ok, State#state{info_tab=Tab}};

handle_call(clean, _From, State) ->
    ets:delete(State#state.info_tab),
    {reply, ok, State};

handle_call(stop, _From, State) ->  {stop, normal, stopped, State};
handle_call(_, _From, State) ->  {reply, [], State}.  
handle_cast(_Msg, State) -> {noreply, State}.  
handle_info(_Info, State) -> {noreply, State}.  
terminate(_Reason, _State) -> ok.  
code_change(_OldVsn, State, _Extra) -> {ok, State}. 

create_ets() ->
    Tab = ets:new(iplib_ip_info, [ordered_set, named_table, protected]),
    Data = json_to_list("ip.db"),
    lists:foreach((fun(Struct) ->
        case tidy_ipinfo(Struct) of
            {Stop, Rec} -> ets:insert(Tab, {Stop, Rec});
            null -> skip
        end
    end), Data),
    Tab.

json_fetch(Type) ->
    File = iplib_utils:priv_dir(iplib) ++ "/" ++ Type,
    {ok, Data} = file:read_file(File),
    Data.

json_to_list(File) ->
    Json = json_fetch(File),
    [Data] = mochijson2:decode(Json),
    Data.
    
tidy_ipinfo(Struct) ->
    try 
        {struct, Data} = Struct,
        %io:format("Stop: ~p ~n", [proplists:get_value(<<"stop">>, Data)])
        Stop = proplists:get_value(<<"stop">>, Data),
        Rec = #iplib_ip_info{
            start=proplists:get_value(<<"start">>, Data),
            stop=proplists:get_value(<<"stop">>, Data),
            address=proplists:get_value(<<"address">>, Data),
            country=proplists:get_value(<<"country">>, Data),
            descr=proplists:get_value(<<"descr">>, Data),
            location=proplists:get_value(<<"location">>, Data),
            netname=proplists:get_value(<<"netname">>, Data),
            network=proplists:get_value(<<"network">>, Data),
            province=proplists:get_value(<<"province">>, Data)
        },
        {Stop, Rec}
    catch _:_ ->
        io:format("Data: [ ~p ], skip ~n", [Struct]),
        null
    end.




