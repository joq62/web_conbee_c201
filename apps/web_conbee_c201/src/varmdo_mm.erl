%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(varmdo_mm).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------



%% sensors
-export([
	 temp_main_house/0,
	 door_main/0,
	 presence_hall/0
	]). 
%% switches
-export([
	 get_switch_heater_livingroom/0,
	 set_switch_heater_livingroom/1
	]). 

%% lights
-export([
	 get_light_hall/0,
	 set_light_hall/1
	]). 


%% ====================================================================
%% External functions
%% ====================================================================

%% sensors
-define(temp_main_house,"8").
-define(temp_outdoor_room,2x).
-define(temp_guest_house,3x).
-define(door_main,"11").
-define(door_kitchen,5x).
-define(presence_hall,"12"). % (turn on hall and kitchen lamp)

%% switches
-define(switch_heater_livingroom,10).
-define(switch_heater_kitchen,11).
-define(switch_heater_asa_bedroom,12).
-define(switch_heater_joakim_bedroom,13).
-define(switch_heater_erika_room,14).
-define(switch_heater_guest_house,15).
-define(switch_heater_outdoor_floor,16). % (på vinden)
-define(switch_tv,17).

%% lights

-define(light_main_entrance,"2"). % (IKEA lamp)
-define(light_hall,21). % (IKEA lamp)
-define(light_kitchen,22). 
-define(light_livingroom_table,23).
-define(light_corner,24).

%% manual switch
-define(main_all_lamps,"3"). 
-define(main_all_heaters,31).

%% --------------------------------------------------------------------
%% Function:sensors
%% Description:  
%% Returns: non
%% --------------------------------------------------------------------
temp_main_house()->
    Result=case rpc:call(node(),lists,keyfind,[?temp_main_house,2,sensors:get_info()],5000) of
	       {badrpc,Reason}->
		   {error,[{badrpc,Reason}]};
	       false->
		   {error,[eexists]};
	       {_,?temp_main_house,"temperature",Temp}->
		   {ok,Temp}
	   end,
    Result.

door_main()->
    Result=case rpc:call(node(),lists,keyfind,[?door_main,2,sensors:get_info()],5000) of
	       {badrpc,Reason}->
		   {error,[{badrpc,Reason}]};
	       false->
		   {error,[eexists]};
	       {_,?door_main,"open",Bool}->
		   case Bool of
		       false->
			   {ok,"closed"};
		       true->
			   {ok,"open"}
		   end
	   end,
    Result.

presence_hall()->
    Result=case rpc:call(node(),lists,keyfind,[?presence_hall,2,sensors:get_info()],5000) of
	       {badrpc,Reason}->
		   {error,[{badrpc,Reason}]};
	       false->
		   {error,[eexists]};
	       {_,?presence_hall,"presence",Presence}->
		  
		   {ok,Presence}
	   end,
    Result.



%% --------------------------------------------------------------------
%% Function:switches
%% Description:  
%% Returns: non
%% --------------------------------------------------------------------
get_switch_heater_livingroom()->
    State=state_switch_heater_livingroom,
    State.

set_switch_heater_livingroom(State)->
    {ok,set_switch_heater_livingroom} .


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
get_light_hall()->
    State=get_light_hall,
    State.

set_light_hall(State)->
    {ok,set_light_hall}.
