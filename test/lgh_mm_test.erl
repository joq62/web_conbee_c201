%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lgh_mm_test).    
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("log.hrl").

%%---------------------------------------------------------------------
%% Records for test
%%


%% --------------------------------------------------------------------
%-compile(export_all).

-export([
	 switch_is_reachable/1,
	 switch_is_on/1,
	 switch_set_on/1,
	 switch_set_off/1
	 
	]).

-export([
	 lamp_is_reachable/1,
	 lamp_is_on/1 %,
%	 lamp_dim_set_on/2,
%	 lamp_color_set_on/2,
%	 lamp_set_off/1
	 
	]).

%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

lamp_is_reachable("lamp_joqroom")->
    tradfri_bulb_e27_ww_806lm:reachable("lamp_joqroom");
lamp_is_reachable("lamp_erika")->
    tradfri_bulb_e27_cws_806lm:reachable("lamp_erika").

lamp_is_on("lamp_joqroom")->
    tradfri_bulb_e27_ww_806lm:is_on("lamp_joqroom");
lamp_is_on("lamp_erika")->
    tradfri_bulb_e27_cws_806lm:is_on("lamp_erika").


    

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


switch_is_on(SwitchId)->
    tradfri_control_outlet:is_on(SwitchId).

switch_is_reachable(SwitchId)->
    tradfri_control_outlet:reachable(SwitchId).
 
switch_set_on(SwitchId)->
    tradfri_control_outlet:set(SwitchId,"on").

switch_set_off(SwitchId)->
    tradfri_control_outlet:set(SwitchId,"off").

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


    
