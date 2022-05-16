%%%-------------------------------------------------------------------
%% @doc web_conbee_c201 public API
%% @end
%%%-------------------------------------------------------------------

-module(web_conbee_c201_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
   
    conbee_init:start(),
    web_init:start(),
    web_conbee_c201_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
