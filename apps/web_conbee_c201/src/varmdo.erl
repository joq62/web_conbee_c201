%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(varmdo). 

-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
-define(SERVER,?MODULE).
-define(CheckIntervall,20*1000).

%% External exports
-export([
	 check/0,
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
		heather_main_house_status,
		heather_guest_house_status
	        	
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
check()-> 
    gen_server:cast(?MODULE, {check}).


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
    StatusHeatherMainHouse="OFF",
    StatusHeatherGuestHouse="OFF",
    
    {ok, #state{heather_main_house_status=StatusHeatherMainHouse,
		heather_guest_house_status=StatusHeatherGuestHouse}}.

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
    {Reply,NewState}=format_text(init,State#state{pid=Pid}),
    {reply, Reply,NewState};


handle_call({websocket_handle,{text, <<"main_house_on">>}},_From,State) ->
    io:format("main_house_on  ~p~n",[{?MODULE,?LINE}]),
    
    tradfri_bulb_e27_ww_806lm:set("lamp_joqroom","on"),

    NewState=State#state{heather_main_house_status="ON"},
    {Reply,NewState}=format_text(NewState),
    {reply, Reply, NewState};
handle_call({websocket_handle,{text, <<"main_house_off">>}},_From,State) ->
    io:format("main_house_off  ~p~n",[{?MODULE,?LINE}]),

    tradfri_bulb_e27_ww_806lm:set("lamp_joqroom","off"),

    NewState=State#state{heather_main_house_status="OFF"},
    {Reply,NewState}=format_text(NewState),
    {reply, Reply, NewState};
handle_call({websocket_handle,{text, <<"guest_house_on">>}},_From,State) ->
    io:format("guest_house_on  ~p~n",[{?MODULE,?LINE}]),

    lgh_mm_test:switch_set_on("switch_lamp_balcony"),
    lgh_mm_test:switch_set_on("switch_lamp_kitchen"),
    lgh_mm_test:switch_set_on("switch_lamp_hall"),

    NewState=State#state{heather_guest_house_status="ON"},
    {Reply,NewState}=format_text(NewState),
    {reply, Reply, NewState};
handle_call({websocket_handle,{text, <<"guest_house_off">>}},_From,State) ->
    io:format("guest_house_off  ~p~n",[{?MODULE,?LINE}]),

    lgh_mm_test:switch_set_off("switch_lamp_balcony"),
    lgh_mm_test:switch_set_off("switch_lamp_kitchen"),
    lgh_mm_test:switch_set_off("switch_lamp_hall"),

    NewState=State#state{heather_guest_house_status="OFF"},
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
handle_cast({check}, State) ->
    NewState=State,
    spawn(fun()->do_check() end),
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
do_check()->
    timer:sleep(?CheckIntervall),
    rpc:cast(node(),?MODULE,check,[]).
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
    StatusHeatherMainHouse=NewState#state.heather_main_house_status,
    StatusHeatherGuestHouse=NewState#state.heather_guest_house_status,

    A=["~s~s~s", [StatusHeatherMainHouse,",",StatusHeatherGuestHouse]],
    {{ok,Type,M,F,A},NewState}.

		  
