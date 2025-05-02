-module(room_manager).
-behaviour(gen_server).

-export([start_link/0, create_room/3, destroy_room/3, list_rooms/1, join_room/2, leave_room/3, broadcast/4, invite/4, accept_invite/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(room, {name, creator, users = []}).
-record(private_room, {name, creator, users = [], invited = []}).

%%% --- Public API ---
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_room(User, RoomName, IsPrivate) ->
    gen_server:call(?MODULE, {create_room, User, RoomName, IsPrivate}).

destroy_room(User, RoomName, IsPrivate) ->
    case IsPrivate of
        true -> gen_server:call(?MODULE, {destroy_private_room, User, RoomName});
        false -> gen_server:call(?MODULE, {destroy_public_room, User, RoomName})
    end.

list_rooms(User) ->
    gen_server:call(?MODULE, {list_rooms, User}).

join_room(User, RoomName) ->
    gen_server:call(?MODULE, {join_room, User, RoomName}).

leave_room(User, RoomName, IsPrivate) ->
    case IsPrivate of
        true -> gen_server:call(?MODULE, {leave_private_room, User, RoomName});
        false -> gen_server:call(?MODULE, {leave_public_room, User, RoomName})
    end.

broadcast(Socket, User, RoomName, Message) ->
    gen_server:cast(?MODULE, {broadcast, Socket, User, RoomName, Message}).

invite(User, UserSocket, RoomName, Receiver) ->
    gen_server:cast(?MODULE, {invite, User, UserSocket, RoomName, Receiver}).

accept_invite(User, RoomName) ->
    gen_server:call(?MODULE, {accept_invite, User, RoomName}).
%%% --- Callbacks ---

init([]) ->
    {ok, #{public => #{}, private => #{}}}.    % Initial state: empty maps private and public rooms (room name -> #room{})

handle_call({create_room, User, RoomName, IsPrivate}, _From, State) ->
    Key = case IsPrivate of
        true -> private;
        false -> public
    end,
    RoomMap = maps:get(Key, State),  % Get the current map for private or public rooms
    case maps:get(RoomName, RoomMap, undefined) of
        undefined -> 
            Room = case IsPrivate of
                true -> #private_room{name = RoomName, creator = User, users = [User], invited = []};
                false -> #room{name = RoomName, creator = User, users = [User]}
            end,
                io:format("[room_manager] New room created: ~p~n", [Room]),
            % Add the room to the correct map (public or private)
            NewRoomMap = RoomMap#{RoomName => Room},
            NewState = State#{Key => NewRoomMap},  % Update the state with the new map
            {reply, {ok, RoomName}, NewState};
        _ ->
            io:format("[room_manager] Room creation failed, already exists: ~p~n", [RoomName]),
            {reply, {error, room_already_exists}, State}
    end;

handle_call({destroy_public_room, User, RoomName}, _From, State) ->
    PublicMap = maps:get(public, State, #{}),
    case maps:find(RoomName, PublicMap) of
        error ->
            io:format("[room_manager] Public room not found: ~p~n", [RoomName]),
            {reply, {error, room_not_found}, State};
        {ok, #room{creator = Creator}} when Creator =:= User ->
            io:format("[room_manager] Public room destroyed: ~p~n", [RoomName]),
            NewPub = maps:remove(RoomName, PublicMap),
            {reply, {ok, destroyed}, State#{public => NewPub}};
        {ok, _Room} ->
            io:format("[room_manager] Cannot destroy public room ~p: not creator~n", [RoomName]),
            {reply, {error, not_creator}, State}
    end;

handle_call({destroy_private_room, User, RoomName}, _From, State) ->
    PrivMap = maps:get(private, State, #{}),
    case maps:find(RoomName, PrivMap) of
        error ->
            io:format("[room_manager] Private room not found: ~p~n", [RoomName]),
            {reply, {error, room_not_found}, State};
        {ok, #private_room{creator = Creator}} when Creator =:= User ->
            io:format("[room_manager] Private room destroyed: ~p~n", [RoomName]),
            NewPriv = maps:remove(RoomName, PrivMap),
            {reply, {ok, destroyed}, State#{private => NewPriv}};
        {ok, _Room} ->
            io:format("[room_manager] Cannot destroy private room ~p: not creator~n", [RoomName]),
            {reply, {error, not_creator}, State}
    end;

handle_call({list_rooms, User}, _From, State) ->
    io:format("[room_manager] All rooms ~p~n", [State]),
    Public = maps:keys(maps:get(public, State, #{})),
    Private = [Name || {Name, #private_room{invited = I, users = U}} <- maps:to_list(maps:get(private, State, #{})),
                        lists:member(User, I) orelse lists:member(User, U)],
    AllRooms = Public ++ Private,
    io:format("[room_manager] List of rooms for ~p: ~p~n", [User, AllRooms]),
    {reply, {ok, AllRooms}, State};   

handle_call({join_room, User, RoomName}, _From, State) ->
    PubMap = maps:get(public, State, #{}),
    case maps:find(RoomName, PubMap) of
        error ->
            io:format("[room_manager] Room join failed, room not found: ~p~n", [RoomName]),
            {reply, {error, room_not_found}, State};
        {ok, #room{users = Users} = Room} ->
            case lists:member(User, Users) of
                true ->
                    io:format("[room_manager] Room join failed, user already in room: ~p~n", [RoomName]),
                    {reply, {error, already_joined}, State};
                false ->
                    io:format("[room_manager] ~p joined public room: ~p~n", [User, RoomName]),
                    NewPubMap  = maps:put(RoomName, Room#room{users = [User | Users]}, PubMap),
                    {reply, {ok, joined}, State#{public => NewPubMap}}
            end
    end;

handle_call({leave_public_room, User, RoomName}, _From, State) ->
    PubMap = maps:get(public, State, #{}),
    case maps:find(RoomName, PubMap) of
        error ->
            io:format("[room_manager] Leave failed, public room not found: ~p~n", [RoomName]),
            {reply, {error, room_not_found}, State};
        {ok, #room{users = Users} = Room} ->
            case lists:member(User, Users) of
                false ->
                    io:format("[room_manager] Leave failed, not in public room: ~p~n", [RoomName]),
                    {reply, {error, user_not_in_room}, State};
                true ->
                    NewPubMap  = maps:put(RoomName, Room#room{users = lists:delete(User, Users)}, PubMap),
                    NewState   = State#{public => NewPubMap},
                    io:format("[room_manager] ~p left public room: ~p~n", [User, RoomName]),
                    {reply, {ok, left}, NewState}
            end
    end;

handle_call({leave_private_room, User, RoomName}, _From, State) ->
    PrivMap = maps:get(private, State, #{}),
    case maps:find(RoomName, PrivMap) of
        error ->
            io:format("[room_manager] Leave failed, private room not found: ~p~n", [RoomName]),
            {reply, {error, room_not_found}, State};
        {ok, #private_room{users = Users} = Room} ->
            case lists:member(User, Users) of
                false ->
                    io:format("[room_manager] Leave failed, not in private room: ~p~n", [RoomName]),
                    {reply, {error, user_not_in_room}, State};
                true ->
                    NewPrivMap  = maps:put(RoomName, Room#private_room{users = lists:delete(User, Users)}, PrivMap),
                    NewState    = State#{private => NewPrivMap},
                    io:format("[room_manager] ~p left private room: ~p~n", [User, RoomName]),
                    {reply, {ok, left}, NewState}
            end
    end;

handle_call({accept_invite, User, RoomName}, _From, State) ->
    io:format("[room_manager] State: ~p~n", [State]),
    PrivMap = maps:get(private, State, #{}),
    case maps:find(RoomName, PrivMap) of
        error ->
            io:format("[room_manager] Accept invite failed, private room not found: ~p~n", [RoomName]),
            {reply, {error, room_not_found}, State};
        {ok, #private_room{invited = Invited, users = Users} = Room} ->
            User,
            case lists:member(User, Invited) of
                false ->
                    io:format("[room_manager] Accept invite failed, user not invited: ~p~n", [RoomName]),
                    {reply, {error, not_invited}, State};
                true ->
                    NewUsers = [User | Users],
                    NewRoom  = Room#private_room{users = NewUsers},
                    NewState = State#{private => maps:put(RoomName, NewRoom, PrivMap)},
                    io:format("[room_manager] ~p accepted invite to private room: ~p~n", [User, RoomName]),
                    {reply, {ok, joined}, NewState}
            end
    end.

%%% --- Cast for Broadcast ---

handle_cast({broadcast, SenderSocket, FromUser, RoomName, Message}, State) ->
    PubMap  = maps:get(public,  State, #{}),
    PrivMap = maps:get(private, State, #{}),

    %% Try public first, then private
    case maps:find(RoomName, PubMap) of
        {ok, #room{users = Users}} ->
            do_broadcast(SenderSocket, FromUser, Message, Users),
            {noreply, State};
        error ->
            case maps:find(RoomName, PrivMap) of
                {ok, #private_room{users = Users}} ->
                    do_broadcast(SenderSocket, FromUser, Message, Users),
                    {noreply, State};
                error ->
                    gen_tcp:send(SenderSocket, <<"Error broadcasting: room not found">>),
                    {noreply, State}
            end
    end;

handle_cast({invite, User, UserSocket, RoomName, Receiver}, State) ->
    io:format("[room_manager] State: ~p~n", [State]),
    PrivMap = maps:get(private, State, #{}),
    case maps:find(RoomName, PrivMap) of
        %% Room doesn’t exist
        error ->
            gen_tcp:send(UserSocket, <<"Room not found">>),
            {noreply, State};

        {ok, #private_room{creator = Creator, invited = Invited} = Room} ->
            %% Only the creator may invite
            case Creator =:= User of
                false ->
                    gen_tcp:send(UserSocket, <<"Only the creator can invite users">>),
                    {noreply, State};

                true ->
                    %% Already invited?
                    ReceiverBin = list_to_binary(Receiver),
                    case lists:member(ReceiverBin, Invited) of
                        true ->
                            gen_tcp:send(UserSocket, <<"User already invited">>),
                            {noreply, State};

                        false ->
                            %% Update invited list
                            NewRoom    = Room#private_room{invited = [ReceiverBin | Invited]},
                            NewPrivMap = maps:put(RoomName, NewRoom, PrivMap),
                            NewState   = State#{private => NewPrivMap},

                            %% Notify the invited user if online
                            case tcp_server:get_socket(ReceiverBin) of
                                {ok, RSock} ->
                                    gen_tcp:send(RSock,
                                        io_lib:format(
                                            "You have been invited to private room ~s by ~s~n",
                                            [RoomName, User]));
                                _ -> ok
                            end,

                            %% Confirm back to inviter
                            gen_tcp:send(UserSocket, <<"User invited successfully!">>),

                            {noreply, NewState}
                    end
            end
    end.    

%% Extracted helper for the common “send to each user” logic
do_broadcast(SenderSocket, FromUser, Message, Users) ->
    case lists:member(FromUser, Users) of
        false ->
            gen_tcp:send(SenderSocket, <<"Error broadcasting: user not in room">>);
        true  ->
            FullMsg = io_lib:format("~s: ~s~n", [FromUser, Message]),
            lists:foreach(
                fun(Username) ->
                    if Username =/= FromUser ->
                        case tcp_server:get_socket(Username) of
                            {ok, Sock} -> gen_tcp:send(Sock, FullMsg);
                            _          -> ok
                        end;
                       true -> ok
                    end
                end,
                Users
            ),
            gen_tcp:send(SenderSocket, <<"Message sent successfully!">>)
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
