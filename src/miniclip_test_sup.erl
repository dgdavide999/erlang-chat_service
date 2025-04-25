%%%-------------------------------------------------------------------
%% @doc miniclip_test top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(miniclip_test_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    io:format("[miniclip_test_sup] Supervisor starting...~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("[miniclip_test_sup] Initializing children...~n"),
    Children = [
        #{id => tcp_server,
          start => {tcp_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [tcp_server]}
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.
