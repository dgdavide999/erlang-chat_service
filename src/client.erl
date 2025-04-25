-module(client).
-export([start/2]).


%% No-arg start using defaults
start(Host, Port) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]) of
        {ok, Sock} ->
            io:format("Connected to ~p:~p~n", [Host, Port]),
            Message = <<"Hello from client!">>,
            ok = gen_tcp:send(Sock, Message),
            io:format("Sent: ~p~n", [Message]),
            case gen_tcp:recv(Sock, 0, 5000) of
                {ok, Reply} ->
                    io:format("Received: ~p~n", [Reply]);
                {error, timeout} ->
                    io:format("No reply within timeout~n");
                {error, closed} ->
                    io:format("Server closed connection~n")
            end;
            %% Uncomment the following line to close the socket
            %% gen_tcp:close(Sock),
            %% dio:format("Disconnected~n");
        {error, Reason} ->
            io:format("Failed to connect: ~p~n", [Reason])
    end.
