%% Mitch Olson
%% Thursday May 14 2015

-module(chat_server).

-export([start/1, start/2]).

%Start takes no arguments, and loads in the user info.
start(Environment) ->
	%start(50010, Environment).
	start(9105, Environment).

%Internal start uses resolver to register bank with name server, and spawns the connect function.	
start(Port, Environment) ->
	IP = myelib:my_ip(),
	io:format("starting ~p server ~p:~p~n",[Environment, IP, Port]),
	{ok, Listen} = gen_tcp:listen(Port,
									[binary,{packet,4},
									{reuseaddr,true},
									{active,true}]),
	%% Now register ourselves with the name server
	triv_tcp_resolver:store(Environment, {IP, Port}),
	spawn(fun() -> connect(Listen) end).

connect(Listen) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	spawn(fun() -> connect(Listen) end),
	connection_loop(Socket).

%Connection loop receives requests from the client.
connection_loop(Socket) ->
	receive
	{tcp, Socket, Bin} ->
		Term = binary_to_term(Bin),
		case Term of
			%Each request has appropriate error handling and saving if a write has been made.
			{login, {Name, Pwd}} -> 
				case user_manager:login(Name, Pwd) of
							{ok, _MailboxID} -> gen_tcp:send(Socket,
																	term_to_binary(login_success)),
																	connection_loop(Socket);
							{error, Reason} -> 	gen_tcp:send(Socket,
													term_to_binary({error, Reason})),
													connection_loop(Socket)
						end;
			{create_user, {Name, Pwd}} ->
				case user_manager:create_user(Name, Pwd) of
							{success} -> gen_tcp:send(Socket,
													term_to_binary(success)),
													connection_loop(Socket);
							{error, Reason} -> 	gen_tcp:send(Socket,
													term_to_binary({error, Reason})),
													connection_loop(Socket)
						end;
			{send_message, {From, To, Subject, Body, Date}} ->
				case user_manager:incoming_message(To, {From, To, Subject, Body, Date}, []) of
							{success} -> gen_tcp:send(Socket,
													term_to_binary(success)),
													connection_loop(Socket);
							{error, FailedDeliveries} -> 	gen_tcp:send(Socket,
													term_to_binary({error, FailedDeliveries})),
													connection_loop(Socket);
							Other -> io:format("Received back from send message ~p~n", [Other]),
								connection_loop(Socket)
						end;
			{check_mail, {Username, Password}} ->
				case user_manager:check_mail(Username, Password) of
							{Messages, NewCount, TotalCount} -> gen_tcp:send(Socket,
													term_to_binary({Messages, NewCount, TotalCount})),
													connection_loop(Socket);
							{error, Reason} -> 	gen_tcp:send(Socket,
													term_to_binary({error, Reason})),
													connection_loop(Socket)
						end;
			{delete_message, {Username, UniqueMessageID}} ->
				case user_manager:delete_individual_message(Username, UniqueMessageID) of
					ok -> gen_tcp:send(Socket,
										term_to_binary(ok)),
											connection_loop(Socket);
					{error, Reason} -> gen_tcp:send(Socket,
										term_to_binary({error, Reason})),
											connection_loop(Socket)
				end;
			{update_photo, {Username, PhotoBin}} ->
				case user_manager:update_photo(Username, PhotoBin) of
							ok -> gen_tcp:send(Socket,
													term_to_binary(ok)),
													connection_loop(Socket);
							{error, Reason} -> 	gen_tcp:send(Socket,
													term_to_binary({error, Reason})),
													connection_loop(Socket)
						end;
			{get_photo, Username} ->
				case user_manager:get_photo(Username) of
							{ok, X} -> gen_tcp:send(Socket,
													term_to_binary({ok, X})),
													connection_loop(Socket);
							{error, Reason} -> 	gen_tcp:send(Socket,
													term_to_binary({error, Reason})),
													connection_loop(Socket);
							{{ok, X}} -> io:format("Third case"),
										gen_tcp:send(Socket,
													term_to_binary({ok, X})),
													connection_loop(Socket)
						end;
			{destroy_user, Username} ->
				case user_manager:destroy_user(Username) of
							{success}-> gen_tcp:send(Socket,
													term_to_binary({ok})),
													connection_loop(Socket);
							{error, Reason} -> 	gen_tcp:send(Socket,
													term_to_binary({error, Reason})),
													connection_loop(Socket);
							{{success}} -> io:format("Third case"),
										gen_tcp:send(Socket,
													term_to_binary({ok})),
													connection_loop(Socket)
						end;
			{clear_inbox, Username} ->
				case user_manager:clear_inbox(Username) of
							{success}-> gen_tcp:send(Socket,
													term_to_binary({ok})),
													connection_loop(Socket);
							{error, Reason} -> 	gen_tcp:send(Socket,
													term_to_binary({error, Reason})),
													connection_loop(Socket);
							{{success}} -> io:format("Third case"),
										gen_tcp:send(Socket,
													term_to_binary({ok})),
													connection_loop(Socket)
						end;
			{get_message, {Username, UniqueMessageID}} ->
				case user_manager:get_individual_message(Username, UniqueMessageID) of
							{ok, Message} -> gen_tcp:send(Socket,
													term_to_binary(Message)),
													connection_loop(Socket);
							{error, Reason} -> 	gen_tcp:send(Socket,
													term_to_binary({error, Reason})),
													connection_loop(Socket)
						end
		end;
	{tcp_closed, Socket} ->
		exit(normal)
	end.
	
	
	

	
