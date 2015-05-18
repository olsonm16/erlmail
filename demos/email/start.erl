-module(start).
-export([start/1, listToString/2]).

start(Browser) ->
	case whereis(chat_client) of
		undefined -> chat_client:start();
		PID -> io:format("~p~n", [PID])
	end,
	io:format("My chat client says I am : ~p~n", [chat_client:isLoggedOn()]),
	running(Browser, chat_client:isLoggedOn()).

running(Browser, IsLoggedOn) ->
    receive
	{Browser, {struct, [{login, [Username, Password]}]}} ->
		Name = erlang:bitstring_to_list(Username),
		Pass = erlang:bitstring_to_list(Password),
		case chat_client:login(Name, Pass) of
			{login_success} ->  loginSuccess(Browser);
			{session_active, Username} -> sessionActive(Browser, Username);
			{error} -> loginFailure(Browser);
			Other -> io:format("Received ~p~n", [Other])
		end,
	    running(Browser, chat_client:isLoggedOn());
	{Browser, {struct, [{create, [Username, Password]}]}} ->
		Name = erlang:bitstring_to_list(Username),
		Pass = erlang:bitstring_to_list(Password),
		io:format("Received ~p ~p ~n", [Name, Pass]),
		io:format("Client says ~p~n", [chat_client:create_user(Name, Pass)]),
		%case chat_client:create_user(Name, Pass) of
		%	{account_created} ->  createSuccess(Browser);
		%	{creation_failure, {error, Reason}} -> createFailure(Browser);
		%	Other -> io:format("Received ~p~n", [Other])
		%end,
		running(Browser, chat_client:isLoggedOn());
	{Browser, {struct, [{get, _}]}} ->
		getEmails(Browser), 
		running(Browser, chat_client:isLoggedOn());
	{Browser, {struct, [{getMessage, UniqueMessageID}]}} ->
		io:format("Received message ID ~p", [UniqueMessageID]),
		Bin = erlang:bitstring_to_list(UniqueMessageID),
		ListToBin = erlang:list_to_binary(Bin),
		io:format("Converted to request ~p", [ListToBin]),
		getOneMessage(Browser, ListToBin),
		running(Browser, chat_client:isLoggedOn());
	{Browser, {struct, [{toNames, RawToList}, {subject, RawSubject}, {message, RawContent}]}} ->
		TempToList = lists:map(fun(A) -> erlang:bitstring_to_list(A) end, RawToList),
		ToList = lists:map(fun(A) -> re:replace(A, "(^\\s+)|(\\s+$)", "", [global,{return,list}]) end, TempToList),
		Subject = erlang:bitstring_to_list(RawSubject),
		Content = erlang:bitstring_to_list(RawContent),
		chat_client:send_message(ToList, Subject, Content, localTimeToString()),
		running(Browser, chat_client:isLoggedOn());
	{Browser, {struct, [{logout, _}]}} ->
		chat_client:logout(),
		running(Browser, chat_client:isLoggedOn());
	{Browser, Other} ->
		io:format("Received other: ~p~n", [Other]),
		running(Browser, chat_client:isLoggedOn())
    end.

localTimeToString() ->
	{{Y, M, D}, {HH, MIN, SEC}} = calendar:local_time(),
	io_lib:format("~p ~p ~p | ~p:~p", [M, D, Y, HH, MIN]).

getOneMessage(Browser, UniqueMessageID) ->
	case chat_client:getOneMessage(UniqueMessageID) of
		{ok, Message} -> buildMessageView(Browser, Message);
		{ok, {error, Reason}} -> io:format("~p~n", [Reason]);
		Other -> io:format("Hit other case.... meh ~p~n", [Other])
	end.

buildMessageView(Browser, {{From, ToList, Subject, Body, Date}}) ->
	Base = ["<table align='center' width='800px' id = 'tableinbox'><thead>
          <tr><th>From</th><th>To</th><th>Subject</th><th>Date</th></tr></thead>"],
	Header = [Base, "<tbody><tr><td>", From, "</td><td>", string:join(ToList, ", "), "</td><td>", Subject, "</td><td>", Date, "</td></tr><tbody></table>"],
	Content = ["<div id = 'message'><p id = 'messagetxt'>", Body, "</p></div>"],
	Browser ! [{cmd, fill_div}, {id, messagehead}, {txt, list_to_binary(Header)} ],
	Browser ! [{cmd, fill_div}, {id, messagebody}, {txt, list_to_binary(Content)} ].


loginSuccess(Browser) -> 
	io:format("Logged on successfully"),
	Browser ! [{cmd, hide_div}, {id, failed}],
	Browser ! [{cmd, hide_div}, {id, newfailed}],
	Browser ! [{cmd, hide_div}, {id, createaccount}],
	Browser ! [{cmd, show_div}, {id, success}].

loginFailure(Browser) ->
	io:format("Logged on failure"),
	Browser ! [{cmd, hide_div}, {id, newfailed}],
	Browser ! [{cmd, show_div}, {id, failed}].

createSuccess(Browser) -> 
	Browser ! [{cmd, hide_div}, {id, createaccount}],
	Browser ! [{cmd, show_div}, {id, newsuccess}].

createFailure(Browser) ->
	Browser ! [{cmd, hide_div}, {id, failed}],
	Browser ! [{cmd, show_div}, {id, newfailed}].

sessionActive(Browser, Username) ->
	Browser ! [{cmd, hide_div}, {id, failed}],
	Browser ! [{cmd, hide_div}, {id, newfailed}],
	Browser ! [{cmd, hide_div}, {id, createaccount}],
	Browser ! [{cmd, hide_div}, {id, success}],
	Txt = list_to_binary(["Session active: ", Username, " already logged on."]),
	Browser ! [{cmd, fill_div}, {id, sessionActive}, {txt, Txt}],
	Browser ! [{cmd, show_div}, {id, sessionActive}],
	Browser ! [{cmd, show_div}, {id, logoutOption}].

getEmails(Browser) -> 
	%io:format("Success"),
	{Messages, NewCount, TotalCount} = chat_client:check_mail(),
	Terms = [["<table align='center' width='800px' id = 'tableinbox'><thead>
          <tr><th>From</th><th>To</th><th>Subject</th><th>Date</th></tr></thead><tbody>"]],
    Bin = buildInbox(Messages, Terms),
    populateInbox(Browser, lists:reverse(Bin), []).


buildInbox([], Terms) ->  ["</tbody></table>" | Terms];
buildInbox([{{From, ToList, Subject, Body, Date}, MessageID} | T], Terms) ->
	%io:format("MessageID: ~p~n", [MessageID]),
    buildInbox(T, [ ["<tr><td>", From, "</td><td>", string:join(ToList, ", "),"</td><td>", Subject, "</td><td>" , Date, "</td><td id = 'messageid' style ='display: none;'>", MessageID, "</td></tr>"] | Terms]).

populateInbox(Browser, [], MasterBinary) -> Browser ! [{cmd, append_div}, {id, messages}, {txt, list_to_binary(lists:reverse(MasterBinary))}];
populateInbox(Browser, [H | T], MasterBinary) ->
	populateInbox(Browser, T, lists:append(lists:reverse(H), MasterBinary)).

listToString([], String) -> String;
listToString([H], String) -> string:concat(String, io_lib:format("~p", [H]));
listToString([H | T], String) -> listToString(T, string:concat(String, io_lib:format(", ~p", [H]))).



