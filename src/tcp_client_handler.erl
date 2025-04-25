%%% src/tcp_client_handler.erl
-module(tcp_client_handler).
-export([handle/1]).

handle(Socket) ->
    inet:setopts(Socket, [{active, false}]),
    loop(Socket),
    gen_tcp:close(Socket),
    io:format("[tcp_client_handler] Connection closed~n").

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("[tcp_client_handler] Received: ~p~n", [Data]),
            gen_tcp:send(Socket, "Hello from server!"),
            loop(Socket);
        {error, closed} ->
            io:format("[tcp_client_handler] Client disconnected~n"),
            ok;
        {error, Reason} ->
            io:format("[tcp_client_handler] Error: ~p~n", [Reason]),
            ok
    end.
