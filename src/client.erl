-module(client).
-export([create_room/3]).

create_room(Host, Port, RoomName) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]) of
        {ok, Sock} ->
            %% Construct the command to create a room
            Command = string:join(["create", "room", RoomName], "|"),
            io:format("Sent create room command: ~p~n", [Command]),
            ok = gen_tcp:send(Sock, list_to_binary(Command)),
            
            %% Receive server response (success or error message)
            case gen_tcp:recv(Sock, 0) of
                {ok, Reply} ->
                    io:format("Received server response: ~p~n", [Reply]);
                {error, timeout} ->
                    io:format("No reply within timeout~n");
                {error, closed} ->
                    io:format("Server closed connection~n")
            end;
        {error, Reason} ->
            io:format("Failed to connect: ~p~n", [Reason])
    end.