%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lumi_sensor_magnet_aq2).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ModelId,"lumi.sensor_magnet.aq2").
-define(Type,"sensors").
%% --------------------------------------------------------------------



%% External exports
-export([
	 is_open/1
	]). 


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

is_open(Name)->
    {ok,List}=lib_conbee:device(?Type,Name),
    [IsOpen]=[maps:get(<<"open">>,StateMap)||{_Name,_NumId,_ModelId,StateMap}<-List,
					   lists:member( <<"open">>,maps:keys(StateMap))],
    IsOpen.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
