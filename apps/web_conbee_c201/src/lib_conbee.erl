%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_conbee).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(Application,conbee_rel).


%% External exports
-export([
	 device/2,
	 all_raw/0,
	 all_info/1,
	 get_reply/2
	]). 


%% ====================================================================
%% External functions
%% ====================================================================
get_reply(ConnPid,StreamRef)->
 %   io:format("~p~n", [{?MODULE,?LINE}]),
 %   StreamRef = gun:get(ConnPid, "/"),
    case gun:await(ConnPid, StreamRef) of
	{response, fin, Status, Headers} ->
%	    io:format(" no_data ~p~n", [{?MODULE,?LINE}]),
	    Body=[no_data];
	{response, nofin, Status, Headers} ->
%	    io:format(" ~p~n", [{?MODULE,?LINE}]),
	    {ok, Body} = gun:await_body(ConnPid, StreamRef),
	    Body
    end,
    {Status, Headers,Body}.
   
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
all_raw()->
    {ok,ConbeeAddr}=application:get_env(ip),
    {ok,ConbeePort}=application:get_env(port),
    {ok,CmdSensors}=application:get_env(cmd_sensors),

    {ok, ConnPid} = gun:open(ConbeeAddr,ConbeePort),
    Ref=gun:get(ConnPid,CmdSensors),
    Result= all_raw(gun:await_body(ConnPid, Ref)),
    ok=gun:close(ConnPid),
    Result.

all_raw({ok,Body})->
    all_raw(Body);
all_raw(Body)->
    Map=jsx:decode(Body,[]),
    maps:to_list(Map).

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
device(Type,WantedName)->
    Result=case [{Name,NumId,ModelId,State}||{Name,NumId,ModelId,State}<-all_info(Type),
					     WantedName=:=Name] of
	       []->
		   {error,[eexists,Type,WantedName]};
	       List->
		   {ok,List}
	   end,
    Result.
	


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
all_info(Type)->
    {ok,ConbeeAddr}=application:get_env(?Application,addr),
    {ok,ConbeePort}=application:get_env(?Application,port),
    {ok,Crypto}=application:get_env(?Application,key),
    all_info(ConbeeAddr,ConbeePort,Crypto,Type).

all_info(ConbeeAddr,ConbeePort,Crypto,Type)->
    extract_info(ConbeeAddr,ConbeePort,Crypto,Type).
  
extract_info(ConbeeAddr,ConbeePort,Crypto,Type)->
    {ok, ConnPid} = gun:open(ConbeeAddr,ConbeePort),
    CmdLights="/api/"++Crypto++"/"++Type,
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
 %   io:format("L=~p~n",[{?MODULE,?LINE,L}]),
    format_info(L,[]).

format_info([],Formatted)->
    Formatted;
format_info([{IdBin,Map}|T],Acc)->
    NumId=binary_to_list(IdBin),
    Name=binary_to_list(maps:get(<<"name">> ,Map)),
    ModelId=binary_to_list(maps:get(<<"modelid">>,Map)),
    State=maps:get(<<"state">>,Map),
    NewAcc=[{Name,NumId,ModelId,State}|Acc],
    format_info(T,NewAcc).

