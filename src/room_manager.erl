-module(room_manager).
-behaviour(gen_server).

-export([start_link/0, create_room/2, destroy_room/2, list_rooms/0, join_room/2, leave_room/2, broadcast/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(room, {name, creator, users = []}).

%%% --- Public API ---
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_room(User, RoomName) ->
    gen_server:call(?MODULE, {create_room, User, RoomName}).

destroy_room(User, RoomName) ->
    gen_server:call(?MODULE, {destroy_room, User, RoomName}).

list_rooms() ->
    gen_server:call(?MODULE, {list_rooms}).

join_room(User, RoomName) ->
    gen_server:call(?MODULE, {join_room, User, RoomName}).

leave_room(User, RoomName) ->
    gen_server:call(?MODULE, {leave_room, User, RoomName}).

broadcast(Socket, User, RoomName, Message) ->
    gen_server:cast(?MODULE, {broadcast, Socket, User, RoomName, Message}).

%%% --- Callbacks ---

init([]) ->
    {ok, #{}}. % Initial state: empty map (room name -> #room{})

handle_call({create_room, User, RoomName}, _From, State) ->
    case maps:get(RoomName, State, undefined) of
        undefined -> 
            Room = #room{name = RoomName, creator = User, users = [User]},
            io:format("[room_manager] New room created: ~p~n", [Room]),
            {reply, {ok, RoomName}, State#{RoomName => Room}};
        _ ->
            io:format("[room_manager] Room creation failed, already exists: ~p~n", [RoomName]),
            {reply, {error, room_already_exists}, State}
    end;

handle_call({destroy_room, User, RoomName}, _From, State) ->
    case maps:find(RoomName, State) of
        error ->
            io:format("[room_manager] Room destruction failed, not found: ~p~n", [RoomName]),
            {reply, {error, room_not_found}, State};
        {ok, #room{creator = Creator} = Room} ->
            case Creator =:= User of
                true ->
                    io:format("[room_manager] Room destroyed: ~p~n", [RoomName]),
                    {reply, {ok, destroyed}, maps:remove(RoomName, State)};
                false ->
                    io:format("[room_manager] Room destruction failed, not the creator: ~p~n", [RoomName]),
                    {reply, {error, not_creator}, State}
            end
    end;

handle_call({list_rooms}, _From, State) ->
    RoomNames = maps:keys(State),
    {reply, {ok, RoomNames}, State};

handle_call({join_room, User, RoomName}, _From, State) ->
    case maps:get(RoomName, State, undefined) of
        undefined ->
            io:format("[room_manager] Room join failed, room not found: ~p~n", [RoomName]),
            {reply, {error, room_not_found}, State};
        #room{} = Room ->
            Users = Room#room.users,
            case lists:member(User, Users) of
                true ->
                    io:format("[room_manager] Room join failed, user already in room: ~p~n", [RoomName]),
                    {reply, {error, already_joined}, State};
                false ->
                    io:format("[room_manager] ~p joined room: ~p~n", [User, RoomName]),
                    UpdatedRoom = Room#room{users = [User | Users]},
                    {reply, {ok, joined}, State#{RoomName => UpdatedRoom}}
            end
    end;

handle_call({leave_room, User, RoomName}, _From, State) ->
    case maps:get(RoomName, State, undefined) of
        undefined ->
            io:format("[room_manager] Leave failed, room not found: ~p~n", [RoomName]),
            {reply, {error, room_not_found}, State};
        #room{} = Room ->
            case lists:member(User, Room#room.users) of
                true ->
                    NewUsers = lists:delete(User, Room#room.users),
                    UpdatedRoom = Room#room{users = NewUsers},
                    NewState = State#{RoomName => UpdatedRoom},
                    io:format("[room_manager] ~p left room: ~p~n", [User, RoomName]),
                    {reply, {ok, left}, NewState};
                false ->
                    io:format("[room_manager] Leave failed: ~p is not in room: ~p~n", [User, RoomName]),
                    {reply, {error, user_not_in_room}, State}
            end
    end.

%%% --- Cast for Broadcast ---

handle_cast({broadcast, SenderSocket, FromUser, RoomName, Message}, State) ->
    case maps:find(RoomName, State) of
        error ->
            gen_tcp:send(SenderSocket, <<"Error broadcasting: room not found">>),
            {noreply, State};
        {ok, #room{users = Users}} ->
            case lists:member(FromUser, Users) of
                false ->
                    %% send error back to the sender
                    gen_tcp:send(SenderSocket, <<"Error broadcasting: user not in room">>),
                    {noreply, State};
                true ->
                    FullMsg = io_lib:format("~s: ~s~n", [FromUser, Message]),
                    lists:foreach(
                        fun(Username) ->
                            if Username =/= FromUser ->
                                case tcp_server:get_socket(Username) of
                                    {ok, Sock} -> gen_tcp:send(Sock, FullMsg);
                                    _ -> ok
                                end;
                               true -> ok
                            end
                        end,
                        Users
                    ),
                    gen_tcp:send(SenderSocket, <<"Message sent successfully!">>),
                    {noreply, State}
            end
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
