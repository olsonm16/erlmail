-module(triv_tcp_resolver).
-export([store/2, destroy/1, lookup/1]).

%%-define(HOST, "localhost").
%%-define(HOST, "137.113.118.30").
-define(HOST, "192.169.167.49").
%-define(PORT, 50000).
-define(PORT, 9100).

store(Key, Value) ->
	case gen_tcp:connect(?HOST, ?PORT,
						[binary,{packet,4},
						{active,true}]) of
	{ok, Socket} ->
		ok = gen_tcp:send(Socket,
							term_to_binary({add,
											Key,Value})),
		gen_tcp:close(Socket),
		{ok, sent};
	{error, _} ->
		{error, connect}
	end.

%Destroys an entry in the name server.	
destroy(Key) ->
	case gen_tcp:connect(?HOST, ?PORT,
						[binary,{packet,4},
						{active,true}]) of
	{ok, Socket} ->
		ok = gen_tcp:send(Socket,
							term_to_binary({destroy,
											Key})),
		gen_tcp:close(Socket),
		{ok, sent};
	{error, _} ->
		{error, connect}
	end.

lookup(Key) ->
	case gen_tcp:connect(?HOST, ?PORT,
						[binary,{packet,4},
						{active,true}]) of
	{ok, Socket} ->
		ok = gen_tcp:send(Socket,
							term_to_binary({lookup,Key})),
		receive
			{tcp, Socket, Data} ->
				gen_tcp:close(Socket),
				{ok, binary_to_term(Data)};
			{tcp_closed, Socket} ->
				{error, closed}
		end;
		{error, _} ->
			{error, connect}
	end.
