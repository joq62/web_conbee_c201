%%%-------------------------------------------------------------------
%% @doc conbee_rel public API
%% @end
%%%-------------------------------------------------------------------
-define(HwConfig,"hw.config").

-module(conbee_init).


-export([start/0,
	 start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok.

start()->
    application:ensure_all_started(gun),
    case code:where_is_file(?HwConfig) of
	non_existing->
	    {error,[non_existing,?HwConfig]};
	AbsFilename ->
	    {ok,I}=file:consult(AbsFilename),
	    {conbee_addr,ConbeeAddr}=lists:keyfind(conbee_addr,1,I),
	    {conbee_port,ConbeePort}=lists:keyfind(conbee_port,1,I),
	    {conbee_key,ConbeeKey}=lists:keyfind(conbee_key,1,I),
	    io:format("~p~n",[{addr,ConbeeAddr,port,ConbeePort,key,ConbeeKey}]),
	    ok=application:set_env([{conbee_rel,[{addr,ConbeeAddr},{port,ConbeePort},{key,ConbeeKey}]}]),
	    rpc:call(node(),conbee_rel_sup,start_link,[],100)
	    
    end.

stop(_State) ->
    ok.

%% internal functions
