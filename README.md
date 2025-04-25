miniclip_test
=====

A simple chat server in Erlang/OTP-25

Build
-----
make sure you have docker and docker-compose installed.

```bash
docker-compose up --build 
```

Test
-----
in a separate terminal run the following command:

```bash
docker exec -it chat_server bash
```
Then, inside the container, launch the Erlang shell with:

```bash
erl -pa _build/default/lib/miniclip_test/ebin
```
Once in the Erlang shell, you can run the client like this:

```erlang
client:start("localhost", 8081).
```