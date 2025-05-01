-module(miniclip_test_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 8081).
-define(HOST, "localhost").

%% Helper functions to facilitate communication with the TCP server
register_user(Socket, User) ->
    Message = string:join(["register", User], "|"),
    io:format("Sent register command: ~p~n", [Message]),
    gen_tcp:send(Socket, list_to_binary(Message)),
    ?assertEqual("User registered successfully!", recv_line(Socket)),
    Socket.

connect_to_server(User) ->
    case gen_tcp:connect(?HOST, ?PORT, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} -> register_user(Socket, User);
        {error, Reason} -> exit({error, Reason})
    end.

recv_line(Socket) ->
    {ok, Bin} = gen_tcp:recv(Socket, 0, 2000),
    binary_to_list(Bin).

%% tests

create_room_test() ->
    io:format("Running Create Room Test~n"),
    Socket = connect_to_server("User1"),
    Message = "create_room|Room1",
    gen_tcp:send(Socket, list_to_binary(Message)),
    ?assertEqual("Room created successfully!", recv_line(Socket)),
    gen_tcp:close(Socket),
    io:format("Create Room Test Passed~n").

create_room_twice_test() ->
    io:format("Running Create Room Twice Test~n"),
    Socket = connect_to_server("User1"),
    Message = "create_room|Room1",
    gen_tcp:send(Socket, list_to_binary(Message)),
    ?assertEqual("Error creating room: room_already_exists", recv_line(Socket)),
    gen_tcp:close(Socket),
    io:format("Create Room Twice Test Passed~n").

list_rooms_test() ->
    io:format("Running List Rooms Test~n"),
    Socket = connect_to_server("User1"),
    Message = "list_rooms",
    gen_tcp:send(Socket, list_to_binary(Message)),
    ?assertEqual("Available rooms: Room1", recv_line(Socket)),
    gen_tcp:close(Socket),
    io:format("List Rooms Test Passed~n").

destroy_room_test() ->
    io:format("Running Destroy Room Test~n"),
    Socket = connect_to_server("User1"),
    Message = "destroy_room|Room1",
    gen_tcp:send(Socket, list_to_binary(Message)),
    ?assertEqual("Room destroyed successfully!", recv_line(Socket)),
    gen_tcp:close(Socket),
    io:format("Destroy Room Test Passed~n").

destroy_someone_else_room_test() ->
    io:format("Running Destroy Room Test~n"),
    Socket1 = connect_to_server("User1"),
    Socket2 = connect_to_server("User2"),

    Message1 = "create_room|Room1",
    gen_tcp:send(Socket1, list_to_binary(Message1)),

    Message2 = "destroy_room|Room1",
    gen_tcp:send(Socket2, list_to_binary(Message2)),
    ?assertEqual("Error destroying room: not_creator", recv_line(Socket2)),
    gen_tcp:close(Socket1),
    gen_tcp:close(Socket2),
    io:format("Destroy Someone Else Room Test Passed~n").

join_room_test() ->
    io:format("Running Join Room Test~n"),
    Socket = connect_to_server("User2"),
    Message = "join_room|Room1",
    gen_tcp:send(Socket, list_to_binary(Message)),
    ?assertEqual("Joined room successfully!", recv_line(Socket)),
    gen_tcp:close(Socket),
    io:format("Join Room Test Passed~n").

join_room_twice_test() ->
    io:format("Running Join Room Test~n"),
    Socket = connect_to_server("User2"),
    Message = "join_room|Room1",
    gen_tcp:send(Socket, list_to_binary(Message)),
    ?assertEqual("Error joining room: already_joined", recv_line(Socket)),
    gen_tcp:close(Socket),
    io:format("Join Room Twice Test Passed~n").

leave_room_test() ->
    io:format("Running Leave Room Test~n"),
    Socket = connect_to_server("User2"),
    Message = "leave_room|Room1",
    gen_tcp:send(Socket, list_to_binary(Message)),
    ?assertEqual("Left room successfully!", recv_line(Socket)),
    gen_tcp:close(Socket),
    io:format("Leave Room Test Passed~n").

leave_room_without_joining_test() ->
    io:format("Running Leave Room Without Joining Test~n"),
    Socket = connect_to_server("User2"),
    Message = "leave_room|Room1",
    gen_tcp:send(Socket, list_to_binary(Message)),
    ?assertEqual("Error leaving room: user_not_in_room", recv_line(Socket)),
    gen_tcp:close(Socket),
    io:format("Leave Room Without Joining Test Passed~n").

broadcast_test() ->
    io:format("Running Broadcast Test~n"),

    % Connect two users
    Socket1 = connect_to_server("User1"),
    Socket2 = connect_to_server("User2"),

    % Make User2 join the room
    gen_tcp:send(Socket2, list_to_binary("join_room|Room1")),
    recv_line(Socket2),  % consume join confirmation
    
    % Now, User1 broadcasts a message
    gen_tcp:send(Socket1, list_to_binary("broadcast|Room1|Hello Everyone!")),    
    % Assert the broadcast response from sender
    ?assertEqual("Message sent successfully!", recv_line(Socket1)),

    % Now check what User2 receives after the broadcast
    ?assertEqual("User1: Hello Everyone!\n", recv_line(Socket2)),
    gen_tcp:close(Socket1),
    gen_tcp:close(Socket2),
    io:format("Broadcast Test Passed~n").

broadcast_nonexisting_room_test() ->
    io:format("Running Broadcast Nonexisting Test~n"),
    Socket = connect_to_server("User1"),
    gen_tcp:send(Socket, list_to_binary("broadcast|Room2|Hello Everyone!")),    
    ?assertEqual("Error broadcasting: room not found", recv_line(Socket)),
    gen_tcp:close(Socket),
    io:format("Broadcast Nonexisting Test Passed~n").

broadcast_without_joining_room_test() ->
    io:format("Running Broadcast Without Joining Room Test~n"),
    % Connect two users
    Socket1 = connect_to_server("User1"),
    Socket2 = connect_to_server("User2"),

    % Make User1 create the room
    gen_tcp:send(Socket1, list_to_binary("create_room|Room2")),
    recv_line(Socket1),  % consume join confirmation


    gen_tcp:send(Socket2, list_to_binary("broadcast|Room2|Hello Everyone!")),    
    ?assertEqual("Error broadcasting: user not in room", recv_line(Socket2)),
    gen_tcp:close(Socket1),
    gen_tcp:close(Socket2),
    io:format("Broadcast Without Joining Room Test Passed~n").

send_message_test() ->
    io:format("Running Send Message Test~n"),
    Socket1 = connect_to_server("User1"),
    Socket2 = connect_to_server("User2"),

    Message = "message|User2|Hello User2!",
    gen_tcp:send(Socket1, list_to_binary(Message)),    
    ?assertEqual("Message sent successfully!", recv_line(Socket1)),

    % Now check what User2 receives after the message
    ?assertEqual("User1: Hello User2!\n", recv_line(Socket2)),
    gen_tcp:close(Socket1),
    gen_tcp:close(Socket2),
    io:format("Send Message Test Passed~n").

send_message_to_nonexisting_user_test() ->
    io:format("Running Send Message to Nonexisting User Test~n"),
    Socket = connect_to_server("User1"),
    Message = "message|User3|Hello User3!",
    gen_tcp:send(Socket, list_to_binary(Message)),    
    ?assertEqual("User not found", recv_line(Socket)),
    gen_tcp:close(Socket),
    io:format("Send Message to Nonexisting User Test Passed~n").