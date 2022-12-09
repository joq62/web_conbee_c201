%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lgh). 

-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
-define(SERVER,?MODULE).
-define(CheckIntervall,10*1000).
-define(TurnOnIndoor,{18,30,00}).
-define(TurnOffIndoor,{22,00,00}).
-define(TurnOnOutDoor,{18,30,00}).
-define(TurnOffOutDoor,{22,30,00}).
-define(BrightnessSilverLamp,15).
-define(BrightnessBlueLamp,45).

%% External exports
-export([
	 check_time/1,
	 wanted_temp/1,
	 temp/1,
	 door/1,
	 motion/1,
	 ping/0
	]).

-export([
	 websocket_init/1,
	 websocket_handle/1,
	 websocket_info/1
	]).

-export([
	 start/0,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		pid,
		lamp_inglasad_status,
		lamps_indoor_status
	        	
	       }).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% Gen server functions

start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).

%% ====================================================================
%% Application handling
%% ====================================================================

%% ====================================================================
%% Support functions
%% ====================================================================
wanted_temp(T)-> 
    gen_server:call(?SERVER, {wanted_temp,T},infinity).

temp(T)-> 
    gen_server:call(?SERVER, {temp,T},infinity).
door(T)-> 
    gen_server:call(?SERVER, {door,T},infinity).
motion(T)-> 
    gen_server:call(?SERVER, {motion,T},infinity).
%% Websocket server functions

websocket_init(S)->
    gen_server:call(?SERVER, {websocket_init,S},infinity).
websocket_handle(Msg)->
    gen_server:call(?SERVER, {websocket_handle,Msg},infinity).
websocket_info(Msg)->
    gen_server:call(?SERVER, {websocket_info,Msg},infinity).

%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


check_time(Status)->
    gen_server:cast(?SERVER, {check_time,Status}).
    
%% ====================================================================
%% Gen Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    % read conbee status
    StatusLampsIndoor=case tradfri_control_outlet:is_on("switch_lamp_kitchen") of
			  true->
			      "ON";
			  false->
			      "OFF"
		      end,
    StatusLampInglasade=case tradfri_bulb_e27_ww_806lm:is_on("lamp_inglasad") of
			  true->
			      "ON";
			  false->
			      "OFF"
		      end,
    
    spawn(fun()->do_check_time() end),
    
    {ok, #state{lamp_inglasad_status=StatusLampInglasade,
		lamps_indoor_status=StatusLampsIndoor}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({websocket_init,Pid},_From,State) ->
    io:format("init websocket ~p~n",[{?MODULE,?LINE,Pid}]),
    {Reply,NewState}=format_text(init,State#state{pid=Pid}),
    {reply, Reply,NewState};


handle_call({websocket_handle,{text, <<"lamp_inglasad_on">>}},_From,State) ->
    io:format("lamp_inglasad_off_on  ~p~n",[{?MODULE,?LINE}]),
    tradfri_bulb_e27_ww_806lm:set("lamp_inglasad","on"),
    tradfri_bulb_e27_ww_806lm:set_bri("lamp_inglasad",?BrightnessSilverLamp),
    NewState=State#state{lamp_inglasad_status="ON"},
    {Reply,NewState}=format_text(NewState),
    {reply, Reply, NewState};

handle_call({websocket_handle,{text, <<"lamp_inglasad_off">>}},_From,State) ->
    io:format("lamp_inglasad_off  ~p~n",[{?MODULE,?LINE}]),
    tradfri_bulb_e27_ww_806lm:set("lamp_inglasad","off"),
    NewState=State#state{lamp_inglasad_status="OFF"},
    {Reply,NewState}=format_text(NewState),
    {reply, Reply, NewState};

handle_call({websocket_handle,{text, <<"lamps_indoor_on">>}},_From,State) ->
    io:format("lamps_indoor_on  ~p~n",[{?MODULE,?LINE}]),

    tradfri_control_outlet:set("switch_lamp_balcony","on"),
    tradfri_control_outlet:set("switch_lamp_kitchen","on"),
    tradfri_control_outlet:set("switch_lamp_hall","on"),

    NewState=State#state{lamps_indoor_status="ON"},
    {Reply,NewState}=format_text(NewState),
    {reply, Reply, NewState};
handle_call({websocket_handle,{text, <<"lamps_indoor_off">>}},_From,State) ->
    io:format("lamps_indoor_off  ~p~n",[{?MODULE,?LINE}]),

    tradfri_control_outlet:set("switch_lamp_balcony","off"),
    tradfri_control_outlet:set("switch_lamp_kitchen","off"),
    tradfri_control_outlet:set("switch_lamp_hall","off"),
 
    NewState=State#state{lamps_indoor_status="OFF"},
    {Reply,NewState}=format_text(NewState),
    {reply, Reply, NewState};





handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call({stopped},_From, State) ->
    Reply=ok,
    {reply, Reply, State}; 


handle_call({not_implemented},_From, State) ->
    Reply=not_implemented,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    io:format("unmatched signal  ~p~n",[{Request,?MODULE,?LINE}]),
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched call",[Request, From])]),
    Reply = {ticket,"unmatched call",Request, From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({check_time,Status}, State) ->
    NewState=State#state{lamp_inglasad_status=Status,
			 lamps_indoor_status=Status},
    spawn(fun()->do_check_time() end),
    {noreply, NewState};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({gun_response,_X1,_X2,_X3,_X4,_X5}, State) ->
    {noreply, State};

handle_info({gun_up,_,_}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_check_time()->
    io:format("  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    timer:sleep(?CheckIntervall),
    %T=time(),
   % Status=case ((T>?TurnOnIndoor) and (T<?TurnOffIndoor)) or (tradfri_on_off_switch:is_on("switch_all")) of
    Status=case (tradfri_on_off_switch:is_on("switch_all")) of
	       false->
		   io:format("false  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
		   tradfri_control_outlet:set("switch_lamp_kitchen","off"),
		   tradfri_control_outlet:set("switch_lamp_hall","off"),
		   "OFF";
	       true ->
		   io:format("true  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
		   tradfri_control_outlet:set("switch_lamp_kitchen","on"),
		   tradfri_control_outlet:set("switch_lamp_hall","on"),
		   tradfri_bulb_e27_ww_806lm:set("lamp_inglasad","on"),
		 
		   "ON"
	   end,

%    case ((T>?TurnOnOutDoor) and (T<?TurnOffOutDoor)) or (tradfri_on_off_switch:is_on("switch_all")) of
    case (tradfri_on_off_switch:is_on("switch_all")) of
	false->
	    io:format("false  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
	    tradfri_bulb_e27_ww_806lm:set("lamp_inglasad","off"),
	    tradfri_bulb_E14_ws_candleopal_470lm:set("blue_lamp_inglasad","off"),
	    tradfri_control_outlet:set("switch_lamp_balcony","off");
	true ->
	    io:format("true  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
	    tradfri_bulb_e27_ww_806lm:set_bri("lamp_inglasad",?BrightnessSilverLamp),
%	    tradfri_bulb_E14_ws_candleopal_470lm:set("blue_lamp_inglasad","on"),
%	    tradfri_bulb_E14_ws_candleopal_470lm:set_bri("blue_lamp_inglasad",?BrightnessBlueLamp),
	    tradfri_control_outlet:set("switch_lamp_balcony","on")
    end,
    io:format("Status  ~p~n",[{Status,?MODULE,?FUNCTION_NAME,?LINE}]),

    rpc:cast(node(),?MODULE,check_time,[Status]).


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
format_text(init,State)->
    format_text(State).


format_text(NewState)->
    Type=text,
    M=io_lib,
    F=format,
    StatusInglasad=NewState#state.lamp_inglasad_status,
    StatusLampsIndoor=NewState#state.lamps_indoor_status,

    A=["~s~s~s", [StatusLampsIndoor,",",StatusInglasad]],
    {{ok,Type,M,F,A},NewState}.

		  
