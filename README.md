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
erlang3 eunit
```


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
% Create a new room
client:create_room("localhost", 8081, "Username", "Room1").

% List all rooms
client:list_rooms("localhost", 8081, "Username").

% Destroy a room
client:destroy_room("localhost", 8081, "Username", "Room1").

% Join a room
client:join_room("localhost", 8081, "Username", "Room1").
```