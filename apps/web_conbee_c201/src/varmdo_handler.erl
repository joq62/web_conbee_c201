-module(varmdo_handler). 


-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req, State) ->
  %  io:format("~p~n",[{?MODULE,?FUNCTION_NAME,?LINE,State}]),
    {cowboy_websocket, Req,State}.

%% web interface MM to server
-define(SERVER,varmdo).
websocket_init(State) ->
    S=self(),    
    {ok,Type,M,F,A}=?SERVER:websocket_init(S),
    {reply, {Type,apply(M,F,A)},State}.

websocket_handle(Msg,State) ->
  %  io:format("Msg ~p~n",[{Msg,?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,Type,M,F,A}=?SERVER:websocket_handle(Msg),
  %  io:format("Replay ~p~n",[{M,F,A, ?MODULE,?FUNCTION_NAME,?LINE}]),
    {reply, {Type,apply(M,F,A)},State}.
  
%%- Interface to server Middle man

websocket_info({ok,Type,M,F,A}, State) ->
    {reply, {Type,apply(M,F,A)},State}.
