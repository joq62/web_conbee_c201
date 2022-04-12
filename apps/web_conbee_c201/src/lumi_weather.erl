%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lumi_weather).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ModelId,"lumi.weather").
-define(Type,"sensors").
%% --------------------------------------------------------------------
%   {"TRADFRI control outlet",
%     "2",
%         #{<<"alert">> => <<"none">>,
%           <<"on">> => false,
%           <<"reachable">> => false}},




%% External exports
-export([
	 temp/1,
	 humidity/1,
	 pressure/1
	]). 


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

temp(Name)->
    {ok,List}=lib_conbee:device(?Type,Name),
    [TempRaw]=[maps:get(<<"temperature">>,StateMap)||{_Name,_NumId,_ModelId,StateMap}<-List,
					   lists:member( <<"temperature">>,maps:keys(StateMap))],
    float_to_list(TempRaw/100,[{decimals,1}]).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

humidity(Name)->
    {ok,List}=lib_conbee:device(?Type,Name),
    [HumidityRaw]=[maps:get(<<"humidity">>,StateMap)||{_Name,_NumId,_ModelId,StateMap}<-List,
					   lists:member( <<"humidity">>,maps:keys(StateMap))],
    float_to_list(HumidityRaw/100,[{decimals,1}])++"%".

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

pressure(Name)->
    {ok,List}=lib_conbee:device(?Type,Name),
    [PressureRaw]=[maps:get(<<"pressure">>,StateMap)||{_Name,_NumId,_ModelId,StateMap}<-List,
					   lists:member( <<"pressure">>,maps:keys(StateMap))],
    integer_to_list(PressureRaw).
    
