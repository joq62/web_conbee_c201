%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lights).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------



%% External exports
-export([
	 get_info_raw/0,
	 get_state/1,
	 set_state/2,

	 get_info/3,
	 get_info/0
	]). 


%% ====================================================================
%% External functions
%% ====================================================================
get_state(Id)->
    
    glurk.

set_state(Id,State)->
    {ok,ConbeeAddr}=application:get_env(conbee_rel,addr),
    {ok,ConbeePort}=application:get_env(conbee_rel,port),
    {ok,Crypto}=application:get_env(conbee_rel,key),

    Cmd="/api/"++Crypto++"/lights/"++Id++"/state",
    Body=case State of
	     "on"->
		 jsx:encode(#{<<"on">> => true});		   
	     "off"->
		 jsx:encode(#{<<"on">> => false})
	 end,
    {ok, ConnPid} = gun:open(ConbeeAddr,ConbeePort),
    StreamRef = gun:put(ConnPid, Cmd, 
			[{<<"content-type">>, "application/json"}],Body),
    Result=lib_conbee:get_reply(ConnPid,StreamRef),
    ok=gun:close(ConnPid),
    Result.
   

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
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
    CmdLights="/api/"++Crypto++"/lights",
    Ref=gun:get(ConnPid,CmdLights),
    Result= get_info(gun:await_body(ConnPid, Ref)),
    ok=gun:close(ConnPid),
    Result.

get_info({ok,Body})->
    get_info(Body);
get_info(Body)->
    Map=jsx:decode(Body,[]),
    format_info(Map).

format_info(Map)->
    L=maps:to_list(Map),
    io:format("L=~p~n",[{?MODULE,?LINE,L}]),
    format_info(L,[]).

format_info([],Formatted)->
    Formatted;
format_info([{IdBin,Map}|T],Acc)->
    Id=binary_to_list(IdBin),
    Name=binary_to_list(maps:get(<<"name">>,Map)),
    ModelId=binary_to_list(maps:get(<<"modelid">>,Map)),
    %
    NewAcc=case get_status(ModelId,Map) of
	       {error,Reason}->
		   io:format("Error ~p~n",[Reason]),
		   Acc;
	{ok,{Key,Value}}->
	    Status={Key,Value},
	    [[{name,Name},{id,Id},{type,ModelId},{status,Status}]|Acc]
    end,
    format_info(T,NewAcc).


get_status("ConBee II",Map)->
    Z=maps:get(<<"state">>,Map),
    true=is_map(Z),
    R=maps:get(<<"reachable">>,Z),
    {ok,{"reachable",R}};  

get_status("TRADFRI bulb E27 WW 806lm",Map)->
    Z=maps:get(<<"state">>,Map),
    true=is_map(Z),
    State=case maps:get(<<"on">>,Z) of
	      true->
		  "on";
	      false->
		  "off"
	  end,
    Brightness=maps:get(<<"bri">>,Z),
    {ok,{"Status",[State,Brightness]}};  

get_status("TRADFRIbulbE14WScandleopal470lm",Map)->
    Z=maps:get(<<"state">>,Map),
    true=is_map(Z),
    State=case maps:get(<<"on">>,Z) of
	      true->
		  "on";
	      false->
		  "off"
	  end,
    Brightness=maps:get(<<"bri">>,Z),
    {ok,{"Status",[State,Brightness]}};  

get_status("TRADFRI control outlet",Map)->
    Z=maps:get(<<"state">>,Map),
    true=is_map(Z),
    State=case maps:get(<<"on">>,Z) of
	      true->
		  "on";
	      false->
		  "off"
	  end,
    {ok,{"State",State}};

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
