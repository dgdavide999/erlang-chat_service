-module(client).
-export([create_room/4, list_rooms/3, destroy_room/4]).

send(Host, Port, User, Message) ->
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


create_room(Host, Port, User, RoomName) ->
    Message = string:join(["create_room", User, RoomName], "|"),
    io:format("Sent create room command: ~p~n", [Message]),
    send(Host, Port, User, Message).

destroy_room(Host, Port, User, RoomName) ->
    Message = string:join(["destroy_room", User, RoomName], "|"),
    io:format("Sent destroy room command: ~p~n", [Message]),
    send(Host, Port, User, Message).

list_rooms(Host, Port, User) ->
    Message = string:join(["list_rooms", User], "|"),
    io:format("Sent list rooms command: ~p~n", [Message]),
    send(Host, Port, User, Message).

join_room(Host, Port, User, RoomName) ->
    Message = string:join(["join_room", User, RoomName], "|"),
    io:format("Sent join room command: ~p~n", [Message]),
    send(Host, Port, User, Message).

leave_room(Host, Port, User, RoomName) ->
    Message = string:join(["leave_room", User, RoomName], "|"),
    io:format("Sent leave room command: ~p~n", [Message]),
    send(Host, Port, User, Message).
