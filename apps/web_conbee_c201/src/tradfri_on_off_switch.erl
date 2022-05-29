%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(tradfri_on_off_switch).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ModelId,"TRADFRI on/off switch").
-define(Type,"sensors").
%% --------------------------------------------------------------------
%   {"TRADFRI control outlet",
%     "2",
%         #{<<"alert">> => <<"none">>,
%           <<"on">> => false,
%           <<"reachable">> => false}},




%% External exports
-export([
	 is_on/1,
	 reachable/1
	 
	]). 


%% ====================================================================
%% External functions
%% ====================================================================


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
is_on(Name)->
    {ok,[{_Name,_NumId,_ModelId,StateMap}]}=lib_conbee:device(?Type,Name),
    case maps:get(<<"buttonevent">>,StateMap) of
	1001->
	    true;
	1002->
	    true;
	1003->
	    true;
	2001 ->
	    false;
	2002 ->
	    false;
	2003 ->
	    false
    end.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
reachable(Name)->
    {ok,[{_Name,_NumId,_ModelId,StateMap}]}=lib_conbee:device(?Type,Name),
     maps:get(<<"reachable">>,StateMap).
