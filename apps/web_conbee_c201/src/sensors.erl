%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(sensors).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------



%% External exports
-export([start/0,
	 get_info_raw/0,
	 get_info/3,
	 get_info/0]). 


%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()-> 

    ok.



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------





%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get_info()->
    {ok,ConbeeAddr}=application:get_env(conbee_rel,addr),
    {ok,ConbeePort}=application:get_env(conbee_rel,port),
    {ok,Crypto}=application:get_env(conbee_rel,key),
    get_info(ConbeeAddr,ConbeePort,Crypto).

get_info(ConbeeAddr,ConbeePort,Crypto)->
    Info=extract_info(ConbeeAddr,ConbeePort,Crypto),
    [{Type,Id,Key,Value}||[{name,Name},{id,Id},{type,Type},{status,{Key,Value}}]<-Info].

extract_info(ConbeeAddr,ConbeePort,Crypto)->
    {ok, ConnPid} = gun:open(ConbeeAddr,ConbeePort),
    CmdSensors="/api/"++Crypto++"/sensors",
    Ref=gun:get(ConnPid,CmdSensors),
    Result= get_info(gun:await_body(ConnPid, Ref)),
    ok=gun:close(ConnPid),
    Result.

get_info({ok,Body})->
    get_info(Body);
get_info(Body)->
  %  io:format("Body ~p~n",[{?MODULE,?LINE,Body}]),
    Map=jsx:decode(Body,[]),
    format_info(Map).

format_info(Map)->
    L=maps:to_list(Map),
%    io:format("L=~p~n",[{?MODULE,?LINE,L}]),
    format_info(L,[]).

format_info([],Formatted)->
    Formatted;
format_info([{IdBin,Map}|T],Acc)->
    Id=binary_to_list(IdBin),
    Name=binary_to_list(maps:get(<<"name">>,Map)),
    Type=binary_to_list(maps:get(<<"type">>,Map)),
    %
    NewAcc=case get_status(Type,Map) of
	       {error,Reason}->
		   io:format("Error ~p~n",[Reason]),
		   Acc;
	{ok,{Key,Value}}->
	    Status={Key,Value},
	    [[{name,Name},{id,Id},{type,Type},{status,Status}]|Acc]
    end,
    format_info(T,NewAcc).


%----------

% [unmatched,"Daylight",sensors,get_status,137]
 
 %   [[{name,"Window / Door Sensor"}, {id,"7"},{type,"ZHAOpenClose"},{status,{"open",false}}],
%-define(door1,"ZHAOpenClose").

%    [{name,"Pressure 6"}, {id,"6"}, {type,"ZHAPressure"}, {status,{"pressure","1045"}}],
%    [{name,"Humidity 5"}, {id,"5"}, {type,"ZHAHumidity"}, {status,{"humidity","22"}}],
%    [{name,"Multi Sensor"}, {id,"4"}, {type,"ZHATemperature"}, {status,{"temperature","31.0"}}],
%-define(indoor_temp_1,"ZHATemperature").
%    [{name,"Presence 3"}, {id,"3"},  {type,"ZHAPresence"}, {status,{"presence",false}}],
%    [{name,"Motion Sensor"},{id,"2"},{type,"ZHALightLevel"},{status,{"daylight",false}}]]


get_status("ZHATemperature",Map)->
    Z=maps:get(<<"state">>,Map),
    true=is_map(Z),
    R=maps:get(<<"temperature">>,Z),
    {ok,{"temperature",float_to_list(R/100,[{decimals,1}])}};  

get_status("ZHAHumidity",Map)->
    Z=maps:get(<<"state">>,Map),
    true=is_map(Z),
    R=maps:get(<<"humidity">>,Z),
    {ok,{"humidity",float_to_list(R/100,[{decimals,0}])}};  
get_status("ZHAPressure",Map)->
    Z=maps:get(<<"state">>,Map),
    true=is_map(Z),
    R=maps:get(<<"pressure">>,Z),
    H=integer_to_list(R),
    {ok,{"pressure",H}};  
get_status("ZHAOpenClose",Map)->
    Z=maps:get(<<"state">>,Map),
    true=is_map(Z),
    R=maps:get(<<"open">>,Z),
    {ok,{"open",R}};  
get_status("ZHALightLevel",Map)->
    Z=maps:get(<<"state">>,Map),
    true=is_map(Z),
    R = maps:get(<<"daylight">>,Z),
    {ok,{"daylight",R}};  
get_status("ZHAPresence",Map)->
    Z=maps:get(<<"state">>,Map),
    true=is_map(Z),
    R=maps:get(<<"presence">>,Z),
    {ok,{"presence",R}};

get_status(Signal,_Map) ->
    {error,[unmatched,Signal,?MODULE,?FUNCTION_NAME,?LINE]}.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get_info_raw()->
    {ok,ConbeeAddr}=application:get_env(ip),
    {ok,ConbeePort}=application:get_env(port),
    {ok,CmdSensors}=application:get_env(key),

    {ok, ConnPid} = gun:open(ConbeeAddr,ConbeePort),
    Ref=gun:get(ConnPid,CmdSensors),
    Result= get_info_raw(gun:await_body(ConnPid, Ref)),
    ok=gun:close(ConnPid),
    Result.

get_info_raw({ok,Body})->
    get_info_raw(Body);
get_info_raw(Body)->
    Map=jsx:decode(Body,[]),
    maps:to_list(Map).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
