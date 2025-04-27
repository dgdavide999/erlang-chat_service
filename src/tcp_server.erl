-module(tcp_server).
-behaviour(gen_server).

-export([start_link/0, register_user/2, get_user_socket/1]).
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
    gen_server:cast(self(), accept),
    State = #{listen_socket => ListenSocket, user_sockets => #{}} ,
    {ok, State}.

%% Public API
register_user(User, Socket) when is_binary(User) ->
    gen_server:cast(?MODULE, {register, User, Socket}).
    
get_user_socket(User) when is_binary(User) ->
    gen_server:call(?MODULE, {get_socket, User}).

%% handle_cast
handle_cast(accept, State = #{listen_socket := ListenSocket}) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            spawn(fun() -> tcp_client_handler:handle(ClientSocket) end),
            %% Continue accepting
            gen_server:cast(self(), accept),
            {noreply, State};
        {error, Reason} ->
            io:format("[tcp_server] Accept error: ~p~n", [Reason]),
            {stop, Reason, State}
    end;

handle_cast({register, User, Socket}, State) ->
    io:format("[tcp_server] Registering user ~p with socket ~p~n", [User, Socket]),
    OldSockets = maps:get(user_sockets, State),
    NewSockets = maps:put(User, Socket, OldSockets),
    NewState = State#{user_sockets => NewSockets},
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_call
handle_call({get_socket, User}, _From, State) ->
    UserSockets = maps:get(user_sockets, State),
    case maps:find(User, UserSockets) of
        {ok, Socket} ->
            {reply, {ok, Socket}, State};
        error ->
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
