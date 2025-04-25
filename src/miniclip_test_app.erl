%%%-------------------------------------------------------------------
%% @doc miniclip_test public API
%% @end
%%%-------------------------------------------------------------------

-module(miniclip_test_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[miniclip_test_app] Application starting~n"),
    miniclip_test_sup:start_link().

stop(_State) ->
    io:format("[miniclip_test_app] Application stopping~n"),
    ok.
