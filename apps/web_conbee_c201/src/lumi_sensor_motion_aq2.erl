%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lumi_sensor_motion_aq2).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ModelId,"lumi.sensor.motion.aq2").
-define(Type,"sensors").
%% --------------------------------------------------------------------
%% #{<<"dark">> => false,<<"daylight">> => false,
%%            <<"lastupdated">> => <<"2022-03-21T22:17:20.339">>,
%%           <<"lightlevel">> => 17994,<<"lux">> => 63}},



%% External exports
-export([
	 is_presence/1,
	 is_dark/1,
	 is_daylight/1,
	 lightlevel/1,
	 lux/1

	]). 


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

lux(Name)->
    {ok,List}=lib_conbee:device(?Type,Name),
    [Info]=[maps:get(<<"lux">>,StateMap)||{_Name,_NumId,_ModelId,StateMap}<-List,
					   lists:member( <<"lux">>,maps:keys(StateMap))],
    Info.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

lightlevel(Name)->
    {ok,List}=lib_conbee:device(?Type,Name),
    [Info]=[maps:get(<<"lightlevel">>,StateMap)||{_Name,_NumId,_ModelId,StateMap}<-List,
					   lists:member( <<"lightlevel">>,maps:keys(StateMap))],
    Info.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

is_daylight(Name)->
    {ok,List}=lib_conbee:device(?Type,Name),
    [Info]=[maps:get(<<"daylight">>,StateMap)||{_Name,_NumId,_ModelId,StateMap}<-List,
					   lists:member( <<"daylight">>,maps:keys(StateMap))],
    Info.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

is_dark(Name)->
    {ok,List}=lib_conbee:device(?Type,Name),
    [Info]=[maps:get(<<"dark">>,StateMap)||{_Name,_NumId,_ModelId,StateMap}<-List,
					   lists:member( <<"dark">>,maps:keys(StateMap))],
    Info.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

is_presence(Name)->
    {ok,List}=lib_conbee:device(?Type,Name),
    [IsPresence]=[maps:get(<<"presence">>,StateMap)||{_Name,_NumId,_ModelId,StateMap}<-List,
					   lists:member( <<"presence">>,maps:keys(StateMap))],
    IsPresence.
