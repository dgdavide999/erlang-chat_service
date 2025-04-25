%%% src/tcp_server.erl
-module(tcp_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2, code_change/3]).

-define(PORT, 8081).

start_link() ->
    io:format("[tcp_server] start_link called~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [
        binary,
        {packet, 0},
        {reuseaddr, true},
        {active, false}
    ]),
    io:format("[tcp_server] Listening on port ~p~n", [?PORT]),
    %% Kick off accept loop
    self() ! accept,
    {ok, ListenSocket}.

handle_info(accept, ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            spawn(fun() -> tcp_client_handler:handle(ClientSocket) end),
            %% Continue accepting
            self() ! accept,
            {noreply, ListenSocket};
        {error, Reason} ->
            io:format("[tcp_server] Accept error: ~p~n", [Reason]),
            {stop, Reason, ListenSocket}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, ListenSocket) ->
    gen_tcp:close(ListenSocket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
