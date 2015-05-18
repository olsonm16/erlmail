%% Mitch Olson
%% Thursday May 14 2015

-module(chat_client).
-export([start/0, login/2, create_user/2, send_message/4, check_mail/0, loop/1, logout/0, isLoggedOn/0, myName/0, getOneMessage/1]).

start() -> register(chat_client, spawn(chat_client, loop, [{null, null}])).

login(Name, Pwd) ->
    rpc({login, {Name, Pwd}}).

create_user(Name, Pwd) ->
    rpc({create_user, {Name, Pwd}}).

send_message(To, Subject, Body, Date) ->
    rpc({send_message, {To, Subject, Body, Date}}).

check_mail() ->
	rpc({check_mail}).

logout() ->
	rpc({logout}).

isLoggedOn() ->
	rpc({isLoggedOn}).

myName() ->
	rpc({myname}).

getOneMessage(UniqueMessageID) ->
	rpc({get_message, UniqueMessageID}).

rpc(Request) ->
	chat_client ! {self(), Request},
	receive
			Reply -> Reply
	end.

loop({Username, Password}) ->
	receive
	{From, {login, {Name, Pwd}}} ->
		case {Username, Password} == {null, null} of
			true -> case makeRequest("UserEnvironment", {login, {Name, Pwd}}) of
						{login_success} -> {From ! {login_success}},
																loop({Name, Pwd});
						{{error, Reason}} -> {From ! {error}}
				end;
			false -> {From ! {session_active, Username}}
			end,
			loop({Username, Password});
	{From, {create_user, {Name, Pwd}}} ->
		case {Username, Password} == {null, null} of
			true -> case makeRequest("UserEnvironment", {create_user, {Name, Pwd}}) of
						{success} -> {From ! {account_created}},
																loop({Name, Pwd});
						{{error, Reason}} -> {From ! {error}}
				end;
			false -> {From ! {chat_client, {login_failure, {Username, "already logged on"}}}}
			end,
			loop({Username, Password});
	{From, {send_message, {To, Subject, Body, Date}}} ->
		case {Username, Password} == {null, null} of
			false -> case makeRequest("UserEnvironment", {send_message, {Username, To, Subject, Body, Date}}) of
						{success} -> {From ! {message_delivered}};
						{error, Reason} -> {From ! {error, Reason}}
				end;
			true -> {From ! {error, "no user logged on"}}
			end,
			loop({Username, Password});
	{From, {check_mail}} ->
		case {Username, Password} == {null, null} of
			false -> case makeRequest("UserEnvironment", {check_mail, {Username, Password}}) of
						{{Messages, NewCount, TotalCount}} -> {From ! {Messages, NewCount, TotalCount}};
						{error, Reason} -> {From ! {inbox_failure, {error, Reason}}}
				end;
			true -> {From ! {error, "no user logged on"}}
		end,
		loop({Username, Password});
	{From, {logout}} ->
		{From ! {logged_out}},
		loop({null, null});
	{From, {isLoggedOn}} ->
		{From ! {Username, Password} /= {null, null}},
		loop({Username, Password});
	{From, {myname}} ->
		{From ! Username},
		loop({Username, Password});
	{From, {get_message, UniqueMessageID}} ->
		case makeRequest("UserEnvironment", {get_message, {Username, UniqueMessageID}}) of
			{error, Reason} -> {From ! {error, Reason}};
			Message -> {From ! {ok, Message}}
		end,
		loop({Username, Password})
	end.
    
makeRequest(Environment, Request) ->
    %% ask the name server where the bank server is
    case triv_tcp_resolver:lookup(Environment) of
	{ok, {value, {Environment, {Host, Port}}}} ->
	    %% connect to the Environment server
	    case gen_tcp:connect(Host, Port,
				 [binary,{packet,4},
				  {active,true}]) of
		{ok, Socket} ->
		    %% encode and send request
		    Reply = term_to_binary(Request), 
		    ok = gen_tcp:send(Socket, Reply),
		    receive
			{tcp, Socket, Data} ->
			    %% receive and decode reply
			    gen_tcp:close(Socket),
			    {binary_to_term(Data)}
		    end;
		{error, _} ->
		    {error, connect}
	    end;
	%Returned by resolver if lookup fails.
	{ok, false} -> {error, "Lookup failed -- Bank server not found in name server"};
	{error, _} ->
	    {error, resolve}
    end.
