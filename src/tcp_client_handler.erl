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

% Handle incoming messages from the client
% This function will parse the command and call the appropriate function in room_manager
% Then it sends a response back to the client
handle_message(Data, Socket) ->
    Command = binary_to_list(Data),
    case parse_command(Command) of
        % Create new room
        {create_room, User, RoomName} ->
        tcp_server:register_user(list_to_binary(User), Socket),
        case room_manager:create_room(User, RoomName) of
            {ok, RoomName} -> 
                gen_tcp:send(Socket, <<"Room created successfully!">>);
            {error, Reason} -> 
                gen_tcp:send(Socket, <<"Error creating room: ", (list_to_binary(atom_to_list(Reason)))/binary>>)
        end;
        % List all rooms
        {list_rooms, User} ->
            tcp_server:register_user(list_to_binary(User), Socket),
            case room_manager:list_rooms() of
            {ok, Rooms} ->
                RoomList = lists:join(", ", Rooms),
                gen_tcp:send(Socket, <<"Available rooms: ", (list_to_binary(RoomList))/binary>>)
        end;
        % Destroy room
        {destroy_room, User, RoomName} ->
            tcp_server:register_user(list_to_binary(User), Socket),
            case room_manager:destroy_room(User, RoomName) of
            {ok, destroyed} -> 
                gen_tcp:send(Socket, <<"Room destroyed successfully!">>);
            {error, Reason} -> 
                gen_tcp:send(Socket, <<"Error destroying room: ", (list_to_binary(atom_to_list(Reason)))/binary>>)
        end;
        % Join room
        {join_room, User, RoomName} ->
            tcp_server:register_user(list_to_binary(User), Socket),
            case room_manager:join_room(User, RoomName) of
            {ok, joined} -> 
                gen_tcp:send(Socket, <<"Joined room successfully!">>);
            {error, Reason} -> 
                gen_tcp:send(Socket, <<"Error joining room: ", (list_to_binary(atom_to_list(Reason)))/binary>>)
        end;
        % Leave room
        {leave_room, User, RoomName} ->
            tcp_server:register_user(list_to_binary(User), Socket),
            case room_manager:leave_room(User, RoomName) of
            {ok, left} -> 
                gen_tcp:send(Socket, <<"Left room successfully!">>);
            {error, Reason} -> 
                gen_tcp:send(Socket, <<"Error leaving room: ", (list_to_binary(atom_to_list(Reason)))/binary>>)
        end;
        _Other ->
            gen_tcp:send(Socket, <<"Unknown command">>)
    end.

% Parse the command from the client input
parse_command(Command) ->
    case string:split(Command, "|", all) of
        ["create_room", User, RoomName] -> {create_room, User, RoomName};
        ["destroy_room", User, RoomName] -> {destroy_room, User, RoomName};
        ["join_room", User, RoomName] -> {join_room, User, RoomName};
        ["leave_room", User, RoomName] -> {leave_room, User, RoomName};
        ["list_rooms", User] -> {list_rooms, User};
        ["send_message", User, RoomName, Message] -> {send_message, User, RoomName, Message};
        _ -> 
            % Unrecognized command
            {unknown, Command}
    end.