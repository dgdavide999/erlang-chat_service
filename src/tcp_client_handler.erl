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
            handle_message(Data, Socket),
            loop(Socket);
        {error, closed} ->
            io:format("[tcp_client_handler] Client disconnected~n"),
            ok;
        {error, Reason} ->
            io:format("[tcp_client_handler] Error: ~p~n", [Reason]),
            ok
    end.

handle_message(Data, Socket) ->
    Command = binary_to_list(Data),
    case parse_command(Command) of
        {create_room, RoomName} -> % Create new room
        case room_manager:create_room(RoomName, Socket) of
            {ok, RoomName} -> 
                gen_tcp:send(Socket, <<"Room created successfully!">>);
            {error, Reason} -> 
                gen_tcp:send(Socket, <<"Error creating room: ", (list_to_binary(atom_to_list(Reason)))/binary>>)
        end;

        _Other ->
            gen_tcp:send(Socket, <<"Unknown command">>)
    end.

parse_command(Command) ->
    case string:split(Command, "|", all) of
        ["create", "room", RoomName] -> {create_room, RoomName};
        ["join", "room", RoomName] -> {join_room, RoomName};
        ["leave", "room"] -> {leave_room};
        ["send", "message", RoomName, Message] -> {send_message, RoomName, Message};
        _ -> 
            % Unrecognized command
            {unknown, Command}
    end.