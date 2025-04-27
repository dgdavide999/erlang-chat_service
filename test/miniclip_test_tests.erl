-module(miniclip_test_tests).
-include_lib("eunit/include/eunit.hrl").

%% Helper functions to facilitate communication with the TCP server
-define(PORT, 8081).
-define(HOST, "localhost").

connect_to_server() ->
    case gen_tcp:connect(?HOST, ?PORT, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} -> Socket;
        {error, Reason} -> exit({error, Reason})
    end.

send_command(Socket, Message) ->
    gen_tcp:send(Socket, list_to_binary(Message)),
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Response} -> Response;
        {error, Reason} -> exit({error, Reason})
    end.

%% Create room test

create_room_test() ->
    io:format("Running Create Room Test~n"),
    Socket = connect_to_server(),
    Message = "create_room|User1|Room1",
    Response = send_command(Socket, Message),
    io:format("Response: ~p~n", [Response]),
    ?assertEqual("Room created successfully!", binary_to_list(Response)),
    gen_tcp:close(Socket),
    io:format("Create Room Test Passed~n").

create_room_twice_test() ->
    io:format("Running Create Room Twice Test~n"),
    Socket = connect_to_server(),
    
    Message = "create_room|User2|Room1",
    Response = send_command(Socket, Message),
    io:format("Second Response: ~p~n", [Response]),
    ?assertEqual("Error creating room: room_already_exists", binary_to_list(Response)),
    
    gen_tcp:close(Socket),
    io:format("Create Room Twice Test Passed~n").

list_rooms_test() ->
    io:format("Running List Rooms Test~n"),
    Socket = connect_to_server(),
    
    Message = "list_rooms|User1",
    Response = send_command(Socket, Message),
    io:format("List Rooms Response: ~p~n", [Response]),
    
    %% We expect the response to contain the Room1
    ?assertMatch("Available rooms: Room1", binary_to_list(Response)),
    
    gen_tcp:close(Socket),
    io:format("List Rooms Test Passed~n").

destroy_room_test() ->
    io:format("Running Destroy Room Test~n"),
    Socket = connect_to_server(),
    Message = "destroy_room|User1|Room1",
    Response = send_command(Socket, Message),
    io:format("Destroy Room Response: ~p~n", [Response]),
    
    %% We expect the room to be destroyed successfully
    ?assertEqual("Room destroyed successfully!", binary_to_list(Response)),
    
    gen_tcp:close(Socket),
    io:format("Destroy Room Test Passed~n").

destroy_someone_else_room_test() ->
    io:format("Running Destroy Room Test~n"),
    Socket = connect_to_server(),

    Message1 = "create_room|User1|Room1",
    send_command(Socket, Message1),

    Message2 = "destroy_room|User2|Room1",
    Response = send_command(Socket, Message2),
    io:format("Destroy Room Response: ~p~n", [Response]),
    
    %% We expect the room to be destroyed successfully
    ?assertEqual("Error destroying room: not_creator", binary_to_list(Response)),
    
    gen_tcp:close(Socket),
    io:format("Destroy Someone Else Room Test Passed~n").

join_room_test() ->
    io:format("Running Join Room Test~n"),
    Socket = connect_to_server(),

    Message2 = "join_room|User2|Room1",
    Response = send_command(Socket, Message2),
    io:format("Join Room Response: ~p~n", [Response]),
    
    %% We expect the room to be joined successfully
    ?assertEqual("Joined room successfully!", binary_to_list(Response)),
    
    gen_tcp:close(Socket),
    io:format("Join Room Test Passed~n").

join_room_twice_test() ->
    io:format("Running Join Room Twice Test~n"),
    Socket = connect_to_server(),

    Message2 = "join_room|User2|Room1",
    Response = send_command(Socket, Message2),
    io:format("Join Room Response: ~p~n", [Response]),
    
    %% We expect the room to be joined successfully
    ?assertEqual("Error joining room: already_joined", binary_to_list(Response)),
    
    gen_tcp:close(Socket),
    io:format("Join Room Twice Test Passed~n").

leave_room_test() ->
    io:format("Running Leave Room Test~n"),
    Socket = connect_to_server(),

    Message2 = "leave_room|User2|Room1",
    Response = send_command(Socket, Message2),
    io:format("Leave Room Response: ~p~n", [Response]),
    
    %% We expect the room to be left successfully
    ?assertEqual("Left room successfully!", binary_to_list(Response)),
    
    gen_tcp:close(Socket),
    io:format("Leave Room Test Passed~n").

leave_room_without_joining_test() ->
    io:format("Running Leave Room Without Joining Test~n"),
    Socket = connect_to_server(),

    Message2 = "leave_room|User2|Room1",
    Response = send_command(Socket, Message2),
    io:format("Leave Room Response: ~p~n", [Response]),
    
    %% We expect the room to be left successfully
    ?assertEqual("Error leaving room: user_not_in_room", binary_to_list(Response)),
    
    gen_tcp:close(Socket),
    io:format("Leave Room Without Joining Test Passed~n").