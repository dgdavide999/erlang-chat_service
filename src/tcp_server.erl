-module(tcp_server).
-behaviour(gen_server).

-export([start_link/0, register_user/2, get_socket/1, get_username/1, message/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PORT, 8081).

start_link() ->
    io:format("[tcp_server] start_link called~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [
        binary,
        {packet, 0},
        {reuseaddr, true},
        {active, false}
    ]),
    io:format("[tcp_server] Listening on port ~p~n", [?PORT]),
    spawn(fun() -> accept_loop(ListenSocket) end),
    State = #{
        listen_socket => ListenSocket,
        user_sockets  => #{},    %% username => socket
        socket_users  => #{}     %% socket   => username
    },
    {ok, State}.


accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            %% Spawn a handler for this client
            spawn(fun() -> tcp_client_handler:handle(ClientSocket) end),
            accept_loop(ListenSocket);
        {error, Reason} ->
            io:format("[tcp_server] Accept error: ~p~n", [Reason]),
            ok
    end.

%% Public API
register_user(User, Socket) when is_binary(User) ->
    io:format("[tcp_client_handler] Registering user: ~p~n", [User]),
    gen_server:call(?MODULE, {register, User, Socket}).
    
get_socket(User) when is_binary(User) ->
    io:format("[tcp_server] get socket"),
    gen_server:call(?MODULE, {get_socket, User}).

get_username(Sock) ->
    io:format("[tcp_server] get username"),
    gen_server:call(?MODULE, {get_username, Sock}).

message(SenderSocket, Sender, Receiver, Message) ->
    io:format("[tcp_server] sending message: ~p to ~p~n", [Message, Receiver]),
    gen_server:cast(?MODULE, {message, SenderSocket, Sender, Receiver, Message}).

%% Callbacks
handle_cast({message, SenderSocket, Sender, Receiver, Message}, State) ->
    UserSockets = maps:get(user_sockets, State, #{}),
    case maps:find(list_to_binary(Receiver), UserSockets) of
        error ->
            % Receiver not found, send error message to SenderSocket
            gen_tcp:send(SenderSocket, <<"User not found">>),    
            {noreply, State};

        {ok, ReceiverSocket} ->
            % Send the message to Receiver
            FullMsg = io_lib:format("~s: ~s~n", [Sender, Message]),
            gen_tcp:send(ReceiverSocket, list_to_binary(FullMsg)),    
            % Send confirmation to Sender
            gen_tcp:send(SenderSocket, <<"Message sent successfully!">>),    
            {noreply, State}
    end;


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({register, User, Socket}, _From, State) ->
    io:format("[tcp_server] Registering user ~p with socket ~p~n", [User, Socket]),
    
    UserSockets = maps:get(user_sockets, State, #{}),
    SocketUsers = maps:get(socket_users, State, #{}),

    %% Remove old socket if user already exists
    OldSocketOpt = maps:find(User, UserSockets),
    SocketUsersCleaned = case OldSocketOpt of
        {ok, OldSocket} -> maps:remove(OldSocket, SocketUsers);
        error -> SocketUsers
    end,

    NewUserSockets = maps:put(User, Socket, UserSockets),
    NewSocketUsers = maps:put(Socket, User, SocketUsersCleaned),

    NewState = State#{user_sockets => NewUserSockets, socket_users => NewSocketUsers},
    {reply, ok, NewState};    


handle_call({get_socket, User}, _From, State) ->
    io:format("[tcp_server] Getting socket for user: ~p~n", [User]),
    UserSockets = maps:get(user_sockets, State, {}),
    case maps:find(User, UserSockets) of
        {ok, Sock} ->
            io:format("[tcp_server] Socket found for ~p: ~p~n", [User, Sock]),
            {reply, {ok, Sock}, State};
        error ->
            io:format("[tcp_server] Socket not found for ~p~n", [User]),
            {reply, {error, user_not_found}, State}
    end;

handle_call({get_username, Sock}, _From, State) ->
    io:format("[tcp_server] Getting username for socket: ~p~n", [Sock]),
    SocketUsers = maps:get(socket_users, State, {}),
    case maps:find(Sock, SocketUsers) of
        {ok, User} ->
            io:format("[tcp_server] user found for ~p: ~p~n", [User, Sock]),
            {reply, {ok, User}, State};
        error ->
            io:format("[tcp_server] User not found for ~p~n", [Sock]),
            {reply, {error, user_not_found}, State}
    end;

handle_call(_Req, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ListenSocket = maps:get(listen_socket, State),
    gen_tcp:close(ListenSocket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
