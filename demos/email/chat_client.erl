%% Mitch Olson
%% Thursday May 14 2015

-module(chat_client).
-export([start/0, login/3, create_user/3, destroy_user/1, clear_inbox/1, send_message/5, check_mail/1, deleteOneMessage/2, loop/1, logout/1, isLoggedOn/1, myName/1, getOneMessage/2, get_photo/1, update_photo/2]).

start() -> spawn(chat_client, loop, [{null, null}]).

%An interface which takes a name and password and makes a remote procedure call with the argument {login, {Name, Pwd}}.
login(Pid, Name, Pwd) ->
    rpc(Pid, {login, {Name, Pwd}}).

%An interface which takes a name and password and makes a remote procedure call with the argument {create_user, {Name, Pwd}}.
create_user(Pid, Name, Pwd) ->
    rpc(Pid, {create_user, {Name, Pwd}}).

%An interface which takes message information and likewise makes a remote procedure call.
send_message(Pid, To, Subject, Body, Date) ->
    rpc(Pid, {send_message, {To, Subject, Body, Date}}).

check_mail(Pid) ->
	rpc(Pid, {check_mail}).

logout(Pid) ->
	rpc(Pid, {logout}).

isLoggedOn(Pid) ->
	rpc(Pid, {isLoggedOn}).

myName(Pid) ->
	rpc(Pid, {myname}).

getOneMessage(Pid, UniqueMessageID) ->
	rpc(Pid, {get_message, UniqueMessageID}).

deleteOneMessage(Pid, UniqueMessageID) ->
	rpc(Pid, {delete_message, UniqueMessageID}).

update_photo(Pid, PhotoBin) ->
	rpc(Pid, {update_photo, PhotoBin}).

get_photo(Pid) ->
	rpc(Pid, {get_photo}).

destroy_user(Pid) ->
	rpc(Pid, {destroy_user}).

clear_inbox(Pid) ->
	rpc(Pid, {clear_inbox}).

rpc(Pid, Request) ->
	Pid ! {self(), Request},
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
			false -> {From ! {login_failure, {Username, "already logged on"}}}
			end,
			loop({Username, Password});
	{From, {send_message, {To, Subject, Body, Date}}} ->
		case {Username, Password} == {null, null} of
			false -> case makeRequest("UserEnvironment", {send_message, {Username, To, Subject, Body, Date}}) of
						{success} -> {From ! {message_delivered}};
						{{error, FailedDeliveries}} -> {From ! {error, FailedDeliveries}}
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
	{From, {delete_message, UniqueMessageID}} ->
		case makeRequest("UserEnvironment", {delete_message, {Username, UniqueMessageID}}) of
			{ok} -> {From ! {message_deleted}};
			{{error, Reason}} -> {From ! {error, Reason}}
		end,
		loop({Username, Password});
	{From, {update_photo, PhotoBin}} ->
		case makeRequest("UserEnvironment", {update_photo, {Username, PhotoBin}}) of
			{error, Reason} -> {From ! {error, Reason}};
			{ok} -> {From ! {ok}}
		end,
		loop({Username, Password});
	{From, {get_photo}} ->
		case makeRequest("UserEnvironment", {get_photo, Username}) of
			{{ok, Bin}} -> {From ! {ok, Bin}};
			{error, Reason} -> {From ! {error, Reason}}
		end,
		loop({Username, Password});
	{From, {destroy_user}} ->
		case makeRequest("UserEnvironment", {destroy_user, Username}) of
			{{ok}} -> {From ! {user_destroyed}};
			{error, Reason} -> {From ! {error, Reason}}
		end,
		loop({null, null});
	{From, {clear_inbox}} ->
		case makeRequest("UserEnvironment", {clear_inbox, Username}) of
			{{ok}} -> {From ! {inbox_cleared}};
			{error, Reason} -> {From ! {error, Reason}}
		end,
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
	{ok, false} -> {error, "Lookup failed -- Chat server not found in name server"};
	{error, _} ->
	    {error, resolve}
    end.
