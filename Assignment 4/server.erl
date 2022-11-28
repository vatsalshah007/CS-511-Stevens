-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);

	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
	case maps:get(ChatName,State#serv_st.chatrooms, false) of
		false ->
			CPID = spawn(chatroom, start_chatroom, [ChatName]),
			case maps:get(ClientPID, State#serv_st.nicks, false) of
				false ->
					error;
				_ClientNickName ->
					UpdatedState = State#serv_st{
						chatrooms = maps:put(ChatName,CPID,State#serv_st.chatrooms),
						registrations = maps:put(ChatName,[],State#serv_st.registrations)
					},
					do_join(ChatName, ClientPID, Ref, UpdatedState)
			end;
		ChatRoomPID ->
			case maps:get(ClientPID, State#serv_st.nicks, false) of
				false ->
					error;
				ClientNickName ->
					ChatRoomPID ! {self(), Ref, register, ClientPID, ClientNickName},
					State#serv_st{
						registrations=maps:put(ChatName,maps:get(ChatName,State#serv_st.registrations) ++ [ClientPID],State#serv_st.registrations)
					}
			end
    end.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    case maps:get(ChatName,State#serv_st.chatrooms,false) of
		false ->
			error;
		ChatRoomPID ->
			UpdatedState = State#serv_st{
				registrations=maps:put(ChatName, lists:filter( fun(CID) -> CID =/= ClientPID end, maps:get(ChatName,State#serv_st.registrations) ),State#serv_st.registrations)
			},
			ClientPID ! {self(), Ref, ack_leave},
			ChatRoomPID ! { self(), Ref, unregister, ClientPID },
			UpdatedState
    end.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
	case maps:values(State#serv_st.nicks) of
		Values ->
			case lists:filter( fun (V) -> V == NewNick end, Values) of
				[] ->
					maps:foreach(fun (ChatName , ChatPID)->{
						case maps:get(ChatName, State#serv_st.registrations, false) of
							false ->
								ok;
							Clients ->
								case lists:filter( fun (C) -> C == ClientPID end, Clients) of
									[] -> ok;
									_ ->
										ChatPID ! {self(), Ref, update_nick, ClientPID, NewNick}
								end
						end
					} end, State#serv_st.chatrooms),
					ClientPID ! {self(), Ref, ok_nick},
					State#serv_st{
						nicks = maps:put(ClientPID , NewNick ,State#serv_st.nicks)
					};
				_Found ->
					ClientPID ! {self(), Ref, err_nick_used},
					State
			end
	end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
	NewNickNames = maps:remove(ClientPID, State#serv_st.nicks),
	NewRegistration = maps:map(fun(_Key, Value) -> lists:delete(ClientPID, Value) end, State#serv_st.registrations),
	maps:foreach(fun (ChatName , ChatPID)->{
		case maps:get(ChatName, State#serv_st.registrations, false) of
			false ->
				ok;
			Clients ->
				case lists:filter( fun (CID) -> CID == ClientPID end, Clients) of 
					[] -> ok;
					_Found ->
						ChatPID ! {self(), Ref, unregister, ClientPID}
				end
		end
	} end, State#serv_st.chatrooms),
	ClientPID ! {self(), Ref, ack_quit},
	State#serv_st{
		nicks = NewNickNames,
		registrations=NewRegistration
	}.
