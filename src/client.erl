-module(client).
-export([connect/3, create_room/1, destroy_room/1, list_rooms/0, join_room/1, leave_room/1, broadcast/2, message/2]).

-define(SOCKET_VAR, client_socket).

connect(Host, Port, User) ->
    %% Connect to the server
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, true}]) of
        {ok, Sock} ->
            erlang:put(?SOCKET_VAR, Sock),
            Listener = spawn(fun() -> listen_loop(Sock) end),
            ok = gen_tcp:controlling_process(Sock, Listener),
            register_user(Sock, User);
        {error, Reason} ->
            io:format("Failed to connect: ~p~n", [Reason])
    end.

register_user(Sock, User) ->
    Message = string:join(["register", User], "|"),
    io:format("Sent register command: ~p~n", [Message]),
    ok = gen_tcp:send(Sock, list_to_binary(Message)),
    ok.

create_room(RoomName) ->
    Message = string:join(["create_room", RoomName], "|"),
    io:format("Sent create room command: ~p~n", [Message]),
    gen_tcp:send(erlang:get(?SOCKET_VAR), list_to_binary(Message)),
    ok.

destroy_room(RoomName) ->
    Message = string:join(["destroy_room", RoomName], "|"),
    io:format("Sent destroy room command: ~p~n", [Message]),
    gen_tcp:send(erlang:get(?SOCKET_VAR), list_to_binary(Message)),
    ok.

list_rooms() ->
    Message = string:join(["list_rooms"], "|"),
    io:format("Sent list rooms command: ~p~n", [Message]),
    gen_tcp:send(erlang:get(?SOCKET_VAR), list_to_binary(Message)),
    ok.

join_room(RoomName) ->
    Message = string:join(["join_room", RoomName], "|"),
    io:format("Sent join room command: ~p~n", [Message]),
    gen_tcp:send(erlang:get(?SOCKET_VAR), list_to_binary(Message)),
    ok.

leave_room(RoomName) ->
    Message = string:join(["leave_room", RoomName], "|"),
    io:format("Sent leave room command: ~p~n", [Message]),
    gen_tcp:send(erlang:get(?SOCKET_VAR), list_to_binary(Message)),
    ok.

broadcast(RoomName, Message) ->
    FullMessage = string:join(["broadcast", RoomName, Message], "|"),
    io:format("Sent broadcast message to room: ~p~n", [RoomName]),
    gen_tcp:send(erlang:get(?SOCKET_VAR), list_to_binary(FullMessage)),
    ok.

message(Receiver, Message) ->
    FullMessage = string:join(["message", Receiver, Message], "|"),
    io:format("Sending message to ~p: ~p~n", [Receiver, Message]),
    gen_tcp:send(erlang:get(?SOCKET_VAR), list_to_binary(FullMessage)),
    ok.


%% --- Listener Management ---

listen_loop(Sock) ->
    receive
        {tcp, Sock, Data} ->
            io:format("[Incoming] ~s~n", [Data]),
            listen_loop(Sock);
        {tcp_closed, Sock} ->
            io:format("[Info] Connection closed.~n", []),
            erlang:erase(?SOCKET_VAR),
            ok;
        {tcp_error, Sock, Reason} ->
            io:format("[Error] Socket error: ~p~n", [Reason]),
            erlang:erase(?SOCKET_VAR),
            ok
    end.