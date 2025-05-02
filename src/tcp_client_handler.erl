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

% This function is used to verify the user before executing any command
% It checks if the user is registered and has a valid socket
with_user(Socket, Fun) ->
    case tcp_server:get_username(Socket) of
        {error, _} ->
            io:format("[tcp_client_handler] User verification failed~n"),
            gen_tcp:send(Socket, <<"User not found">>);
        {ok, User} ->
            Fun(User)
    end.
    

% Handle incoming messages from the client
% This function will parse the command and call the appropriate function in room_manager
% Then it sends a response back to the client
handle_message(Data, Socket) ->
    Command = binary_to_list(Data),
    case parse_command(Command) of

        % Register user (no lookup needed)
        {register, User} ->
            tcp_server:register_user(list_to_binary(User), Socket),
            gen_tcp:send(Socket, <<"User registered successfully!">>);

        % Create room
        {create_room, RoomName, IsPrivate} ->
            with_user(Socket, fun(User) ->
                case room_manager:create_room(User, RoomName, IsPrivate) of
                    {ok, _} -> gen_tcp:send(Socket, <<"Room created successfully!">>);
                    {error, Reason} -> gen_tcp:send(Socket, <<"Error creating room: ", (list_to_binary(atom_to_list(Reason)))/binary>>)
                end
            end);

        % List rooms
        {list_rooms} ->
            with_user(Socket, fun(User) ->
                case room_manager:list_rooms(User) of
                    {ok, Rooms} ->
                        RoomList = case Rooms of
                            [] -> <<"No rooms available">>;
                            _ -> <<"Available rooms: ", (list_to_binary(lists:join(", ", Rooms)))/binary>>
                        end,
                        gen_tcp:send(Socket, RoomList);
                    {error, Reason} ->
                        gen_tcp:send(Socket, <<"Error listing rooms: ", (list_to_binary(atom_to_list(Reason)))/binary>>)
                end
            end);

        % Destroy room
        {destroy_room, RoomName, IsPrivate} ->
            with_user(Socket, fun(User) ->
                case room_manager:destroy_room(User, RoomName, IsPrivate) of
                    {ok, destroyed} -> gen_tcp:send(Socket, <<"Room destroyed successfully!">>);
                    {error, Reason} -> gen_tcp:send(Socket, <<"Error destroying room: ", (list_to_binary(atom_to_list(Reason)))/binary>>)
                end
            end);

        % Join room
        {join_room, RoomName} ->
            with_user(Socket, fun(User) ->
                case room_manager:join_room(User, RoomName) of
                    {ok, joined} -> gen_tcp:send(Socket, <<"Joined room successfully!">>);
                    {error, Reason} -> gen_tcp:send(Socket, <<"Error joining room: ", (list_to_binary(atom_to_list(Reason)))/binary>>)
                end
            end);

        % Leave room
        {leave_room, RoomName, IsPrivate} ->
            with_user(Socket, fun(User) ->
                case room_manager:leave_room(User, RoomName, IsPrivate) of
                    {ok, left} -> gen_tcp:send(Socket, <<"Left room successfully!">>);
                    {error, Reason} -> gen_tcp:send(Socket, <<"Error leaving room: ", (list_to_binary(atom_to_list(Reason)))/binary>>)
                end
            end);

        % Broadcast
        {broadcast, RoomName, Message} ->
            with_user(Socket, fun(User) ->
                io:format("[client-handler] sending a message~n"),
                room_manager:broadcast(Socket, User, RoomName, Message)
            end);

        % Message to another user
        {message, Receiver, Message} ->
            with_user(Socket, fun(User) ->
                io:format("[client-handler] sending a message to ~p~n", [Receiver]),
                tcp_server:message(Socket, User, Receiver, Message)
            end);

        _Other ->
            gen_tcp:send(Socket, <<"Unknown command">>)
    end.


% Parse the command from the client input
parse_command(Command) ->
    case string:split(Command, "|", all) of
        ["register", User] -> {register, User};
        ["create_room", RoomName, "private"] -> {create_room, RoomName, true};
        ["create_room", RoomName, "public"] -> {create_room, RoomName, false};
        ["destroy_room", RoomName, "private"] -> {destroy_room, RoomName, true};
        ["destroy_room", RoomName, "public"] -> {destroy_room, RoomName, false};
        ["join_room", RoomName] -> {join_room, RoomName};
        ["leave_room", RoomName, "private"] -> {leave_room, RoomName, true};
        ["leave_room", RoomName, "public"] -> {leave_room, RoomName, false};
        ["list_rooms"] -> {list_rooms};
        ["broadcast", RoomName, Message] -> {broadcast, RoomName, Message};
        ["message", Receiver, Message] -> {message, Receiver, Message};
        _ -> 
            % Unrecognized command
            {unknown, Command}
    end.