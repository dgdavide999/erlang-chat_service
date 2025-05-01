miniclip_test
=====

A simple chat server in Erlang/OTP-25

Build
-----
make sure you have docker and docker-compose installed.

```bash
docker-compose up --build 
```

Automatic Tests
-----

in a separate terminal run the following command:

```bash
docker exec -it chat_server bash
```
Then, inside the container, launch the Erlang test suite with:

```bash
rebar3 eunit
```

Manual Tests
-----

in a separate terminal run the following command:

```bash
docker exec -it chat_server bash
```
Then, inside the container, launch the Erlang shell with:

```bash
erl -pa _build/default/lib/miniclip_test/ebin
```
Once in the Erlang shell, you can send one of the following commands to the server:
 
```erlang

% Login to the server
 client:connect("localhost", 8081, "Username").

% Create a new room
client:create_room("Room1").

% List all rooms
client:list_rooms().

% Destroy a room
client:destroy_room("Room1").

% Join a room
client:join_room("Room1").

% Leave a room
client:leave_room("Room1").

% Send a broadcast message to a room
client:broadcast("Room1", "Hello, world!").

% Send a private message to a user
client:message("Receiver", "Hello, Receiver").
```