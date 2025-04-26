-module(client).
-export([create_room/3, list_rooms/2]).

send(Host, Port, Message) ->
    %% Connect to the server
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]) of
        {ok, Sock} ->
            %% Send the message
            ok = gen_tcp:send(Sock, list_to_binary(Message)),
            
            %% Receive server response
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


create_room(Host, Port, RoomName) ->
    Message = string:join(["create_room", RoomName], "|"),
    io:format("Sent create room command: ~p~n", [Message]),
    send(Host, Port, Message).

list_rooms(Host, Port) ->
    Message = "list_rooms",
    io:format("Sent list rooms command: ~p~n", [Message]),
    send(Host, Port, Message).

join_room(Host, Port, RoomName) ->
    Message = string:join(["join_room", RoomName], "|"),
    io:format("Sent join room command: ~p~n", [Message]),
    send(Host, Port, Message).

leave_room(Host, Port) ->
    Message = "leave_room",
    io:format("Sent leave room command: ~p~n", [Message]),
    send(Host, Port, Message).