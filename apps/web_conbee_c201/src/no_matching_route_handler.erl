-module(no_matching_route_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).
 
init(Req, State) ->
 
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {reply, {text,io_lib:format("~s", ["error in no_matching_route"])},State }.

websocket_handle(Other, State) ->  %Ignore
    io:format("[Other,State~p~n",[{?MODULE,?LINE,Other,State}]),
    {ok, State}.


websocket_info({text, Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(_Other, State) ->
    {ok, State}.
