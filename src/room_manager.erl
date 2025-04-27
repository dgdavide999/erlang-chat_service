-module(room_manager).
-behaviour(gen_server).

-export([start_link/0, create_room/2, destroy_room/2, list_rooms/0, join_room/2, leave_room/2, broadcast/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% room: name, creator, users (sockets)
-record(room, {name, creator, users = []}).

%%% --- Public API ---
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_room(User, RoomName) ->
    gen_server:call(?MODULE, {create_room, User, RoomName}).

destroy_room(User, RoomName) ->
    gen_server:call(?MODULE, {destroy_room, User, RoomName}).

list_rooms() ->
    gen_server:call(?MODULE, list_rooms).

join_room(User, RoomName) ->
    gen_server:call(?MODULE, {join_room, User, RoomName}).

leave_room(User, RoomName) ->
    gen_server:call(?MODULE, {leave_room, User, RoomName}).

broadcast(User, RoomName, Message) ->
    gen_server:cast(?MODULE, {broadcast, User, RoomName, Message}).

%%% --- Callbacks ---

init([]) ->
    {ok, #{}}. % Initial state: empty map (room name -> #room{})

handle_call({create_room, User, RoomName}, _From, State) ->
    case maps:get(RoomName, State, undefined) of
        undefined -> 
            Room = #room{name=RoomName, creator=User, users=[User]},
            io:format("[room_manager] new Room: ~p~n", [Room]),
            {reply, {ok, RoomName}, State#{RoomName => Room}};
        _Existing ->
            io:format("[room_manager] room creation failed, Room already exists: ~p~n", [RoomName]),
            {reply, {error, room_already_exists}, State}
    end;

handle_call({destroy_room, User, RoomName}, _From, State) ->
    case maps:get(RoomName, State) of
        undefined ->
            {reply, {error, room_not_found}, State};
        #room{creator = Creator} = Room ->
            case Creator =:= User of
                true ->
                {reply, {ok, destroyed}, maps:remove(RoomName, State)};
                false ->
                {reply, {error, not_creator}, State}
            end
    end;

handle_call(list_rooms, _From, State) ->
    RoomNames = maps:keys(State),
    {reply, {ok, RoomNames}, State};

handle_call({join_room, User, RoomName}, _From, State) ->
    case maps:get(RoomName, State) of
        undefined ->
            {reply, {error, room_not_found}, State};
        #room{} = Room ->
            UpdatedRoom = Room#room{users = lists:usort([User | Room#room.users])},
            {reply, {ok, joined}, State#{RoomName => UpdatedRoom}}
    end;

handle_call({leave_room, User}, _From, State) ->
    NewState = maps:map(fun(_RoomName, Room) ->
                                NewUsers = lists:delete(User, Room#room.users),
                                Room#room{users = NewUsers}
                        end, State),
    {reply, ok, NewState}.

handle_cast({broadcast, FromUser, RoomName, Message}, State) ->
    case maps:get(RoomName, State, undefined) of
        undefined ->
            {noreply, State};
        #room{users = Users} ->
            %% For each user in the room, send to everyone *except* the sender
            lists:foreach(
                fun(Username) ->
                    if
                        Username =/= FromUser ->
                            %% look up their socket
                            case tcp_server:get_user_socket(Username) of
                                {ok, Socket} ->
                                    gen_tcp:send(Socket, Message);
                                {error, _} ->
                                    ok
                            end;
                        true ->
                            ok
                    end
                end,
                Users
            ),
            {noreply, State}
    end.
    

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
