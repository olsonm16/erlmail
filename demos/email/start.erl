-module(start).
-export([start/1, listToString/2]).

start(Browser) ->
	running(Browser, null).

makeNewSession(Browser) ->
	PID = chat_client:start(),
	running(Browser, PID).	

running(Browser, ChatClientInstance) ->
	io:format("Running run function"),
	io:format("Chat Client Instance is ~p~n", [ChatClientInstance]),
	%case chat_client:isLoggedOn(ChatClientInstance) of
	%	true -> Browser ! [{cmd, clientIsLoggedOn}];
	%	false -> Browser ! [{cmd, noClientLoggedOn}]
	%end,
    receive
	{Browser, {struct, [{cookie, ProcessID}]}} ->
		io:format("Receiving ProcessID from Browser ~p~n", [ProcessID]),
		ListToBin = erlang:binary_to_list(ProcessID),
		io:format("Converted ListToBin from Browser ~p~n", [ListToBin]),
		BadClientPID = processtools:clientPID(ListToBin),
		ClientPID =  re:replace(BadClientPID, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
		io:format("Result from process tools, ~p~n", [ClientPID]),
		case processtools:findProcessFromString(ClientPID) of
			[] -> running(Browser, chat_client:start());
			PID -> running(Browser, PID)
		end;

	{Browser, {struct, [{login, [Username, Password]}]}} ->
		Name = erlang:bitstring_to_list(Username),
		Pass = erlang:bitstring_to_list(Password),
		io:format("Login request. ~p ~p ~p ~n", [Username, Password, ChatClientInstance]),
		case chat_client:login(ChatClientInstance, Name, Pass) of
			{login_success} ->  loginSuccess(Browser, ChatClientInstance);
			{session_active, Username} -> sessionActive(Browser, Username);
			{error} -> loginFailure(Browser);
			Other -> io:format("Received ~p~n", [Other])
		end,
	    running(Browser, ChatClientInstance);

	{Browser, {struct, [{create, [Username, Password]}]}} ->
		Name = erlang:bitstring_to_list(Username),
		Pass = erlang:bitstring_to_list(Password),
		case chat_client:create_user(ChatClientInstance, Name, Pass) of
			{account_created} ->  loginSuccess(Browser, ChatClientInstance);
			{error} -> createFailure(Browser)
		end,
		running(Browser, ChatClientInstance);

	{Browser, {struct, [{get, Req}]}} ->
		io:format("Get request"),
		getEmails(Browser, ChatClientInstance), 
		getSavedBackground(Browser, ChatClientInstance),
		Browser ! [{cmd, fill_div}, {id, whoami}, {txt, list_to_binary(["Welcome, ", chat_client:myName(ChatClientInstance)])}],
		running(Browser, ChatClientInstance);

	{Browser, {struct, [{getMessage, UniqueMessageID}]}} ->
		Bin = erlang:bitstring_to_list(UniqueMessageID),
		ListToBin = erlang:list_to_binary(Bin),
		getOneMessage(Browser, ListToBin, ChatClientInstance),
		running(Browser, ChatClientInstance);

	{Browser, {struct, [{toNames, RawToList}, {subject, RawSubject}, {message, RawContent}]}} ->
		TempToList = lists:map(fun(A) -> erlang:bitstring_to_list(A) end, RawToList),
		ToList = lists:map(fun(A) -> re:replace(A, "(^\\s+)|(\\s+$)", "", [global,{return,list}]) end, TempToList),
		Subject = erlang:bitstring_to_list(RawSubject),
		Content = erlang:bitstring_to_list(RawContent),
		case chat_client:send_message(ChatClientInstance, ToList, Subject, Content, localTimeToString()) of
			{message_delivered} -> sendSuccess(Browser);
			{error, FailedDeliveries} -> sendFailure(Browser, FailedDeliveries)
		end,
		running(Browser, ChatClientInstance);

	{Browser, {struct, [{logout, _}]}} ->
		chat_client:logout(ChatClientInstance),
		running(Browser, ChatClientInstance);

	{Browser,{struct, [{backgroundImage, Bin}]}} ->
		updateSavedBackground(Browser, Bin, ChatClientInstance),
		running(Browser, ChatClientInstance);

	{Browser, {struct, [{clearinbox, _}]}} ->
		chat_client:clear_inbox(ChatClientInstance),
		running(Browser, ChatClientInstance);

	{Browser, {struct, [{destroyuser, _}]}} ->
		case chat_client:destroy_user(ChatClientInstance) of
			{user_destroyed} -> running(Browser, false);	
			{error, Reason} -> io:format("Deletion problem ~p~p~n", [error, Reason])
		end;

	{Browser, {struct, [{deleteID, CurrentUniqueMessageID}]}} ->
		Bin = erlang:bitstring_to_list(CurrentUniqueMessageID),
		ListToBin = erlang:list_to_binary(Bin),
		deleteOneMessage(Browser, ListToBin, ChatClientInstance),
		running(Browser, ChatClientInstance);

	{Browser, Other} ->
		io:format("Received other: ~p~n", [Other]),
		running(Browser, ChatClientInstance)
    end.

localTimeToString() ->
	{{Y, M, D}, {HH, MIN, SEC}} = calendar:local_time(),
	StringMin = io_lib:format("~p", [MIN]),
	Length = length(lists:nth(1, StringMin)),
	case (HH > 12) of
		true -> DAYNITE = pm, NEW_HH = HH - 12;
		false -> DAYNITE = am, NEW_HH = HH
	end,
	case Length == 1 of
		true -> io_lib:format("~p/~p/~p | ~p:0~p ~p", [M, D, Y, NEW_HH, MIN, DAYNITE]);
		false -> io_lib:format("~p/~p/~p | ~p:~p ~p", [M, D, Y, NEW_HH, MIN, DAYNITE])
	end.

getSavedBackground(Browser, ChatClientInstance) ->
	case chat_client:get_photo(ChatClientInstance) of
		{ok, GoodFormat} -> Browser ! [{cmd, updateBackground}, {txt, GoodFormat}];
		{error, Reason} -> io:format("~p ~p ~n", [error, Reason])
	end.

updateSavedBackground(Browser, Bin, ChatClientInstance) ->
	BinList = [Bin],
	GoodFormat = list_to_binary(BinList),
	case chat_client:update_photo(ChatClientInstance, GoodFormat) of
		{{ok, GoodFormat}} -> Browser ! [{cmd, updateBackground}, {txt, GoodFormat}];
		{ok, GoodFormat} -> Browser ! [{cmd, updateBackground}, {txt, GoodFormat}];
		{ok} -> [];
		{error, Reason} -> io:format("~p ~p ~n", [error, Reason])
	end.

getOneMessage(Browser, UniqueMessageID, ChatClientInstance) ->
	case chat_client:getOneMessage(ChatClientInstance, UniqueMessageID) of
		{ok, Message} -> buildMessageView(Browser, Message, UniqueMessageID);
		{ok, {error, Reason}} -> io:format("~p~n", [Reason]);
		Other -> io:format("~p~n", [Other])
	end.

buildMessageView(Browser, {{From, ToList, Subject, Body, Date}}, UniqueMessageID) ->
	Base = ["<table align='center' width='800px' id = 'tableinbox'><thead>
          <tr><th>From</th><th>To</th><th>Subject</th><th>Date</th></tr></thead>"],
	Header = [Base, "<tbody><tr id='currentmessageview'><td>", From, "</td><td>", string:join(ToList, ", "), "</td><td>", Subject, "</td><td>", Date, "</td><td id = 'messageid' style ='display: none;'>", UniqueMessageID, "</td></tr><tbody></table>"],
	Content = ["<div id = 'message'><p id = 'messagetxt'>", Body, "</p></div>"],
	Browser ! [{cmd, fill_div}, {id, messagehead}, {txt, list_to_binary(Header)} ],
	Browser ! [{cmd, fill_div}, {id, messagebody}, {txt, list_to_binary(Content)} ],
	Browser ! [{cmd, hide_div}, {id, mail}],
	Browser ! [{cmd, setID}, {txt, UniqueMessageID}].

deleteOneMessage(Browser, ListToBin, ChatClientInstance) ->
	case chat_client:deleteOneMessage(ChatClientInstance, ListToBin) of
		{message_deleted} -> Browser ! [{cmd, messageDeleted}];
		{error, Reason} -> io:format("~p ~p ~n", [error, Reason])
	end.

sendSuccess(Browser) ->
	Browser ! [{cmd, messageSent}].
	%Browser ! [{cmd, hide_div}, {id, newmessage}],
	%Browser ! [{cmd, show_div}, {id, mail}],
	%Browser ! [{cmd, show_div}, {id, messages}].

sendFailure(Browser, FailedDeliveries) ->
	FailText = ["Sorry, your message failed to send to the following people (the user names do not exist): ", string:join(FailedDeliveries, ", ")],
	Browser ! [{cmd, append_div}, {id, faileddeliveries}, {txt, list_to_binary(FailText)}],
	Browser ! [{cmd, messageNotSent}].

loginSuccess(Browser, ChatClientInstance) ->
	CookieVal = io_lib:format("~p~n", [ChatClientInstance]), 
	Browser ! [{cmd, toInbox}, {id, CookieVal}].
	%Browser ! [{cmd, hide_div}, {id, failed}],
	%Browser ! [{cmd, hide_div}, {id, newfailed}],
	%Browser ! [{cmd, hide_div}, {id, createaccount}],
	%Browser ! [{cmd, show_div}, {id, success}].

loginFailure(Browser) ->
	Browser ! [{cmd, hide_div}, {id, newfailed}],
	Browser ! [{cmd, show_div}, {id, failed}].

createSuccess(Browser) -> 
	Browser ! [{cmd, hide_div}, {id, createaccount}],
	Browser ! [{cmd, show_div}, {id, newsuccess}].

createFailure(Browser) ->
	Browser ! [{cmd, hide_div}, {id, failed}],
	Browser ! [{cmd, show_div}, {id, newfailure}].

sessionActive(Browser, Username) ->
	Browser ! [{cmd, hide_div}, {id, failed}],
	Browser ! [{cmd, hide_div}, {id, newfailed}],
	Browser ! [{cmd, hide_div}, {id, createaccount}],
	Browser ! [{cmd, hide_div}, {id, success}],
	Txt = list_to_binary(["Session active: ", Username, " already logged on."]),
	Browser ! [{cmd, fill_div}, {id, sessionActive}, {txt, Txt}],
	Browser ! [{cmd, show_div}, {id, sessionActive}],
	Browser ! [{cmd, show_div}, {id, logoutOption}].

getEmails(Browser, ChatClientInstance) -> 
	{Messages, NewCount, TotalCount} = chat_client:check_mail(ChatClientInstance),
	Terms = [["<table align='center' width='800px' id = 'tableinbox'><thead>
          <th>From</th><th>To</th><th>Subject</th><th>Date</th></thead><tbody>"]],
    Bin = buildInbox(Messages, Terms),
    populateInbox(Browser, lists:reverse(Bin), []).


buildInbox([], Terms) ->  ["</tbody></table>" | Terms];
buildInbox([{{From, ToList, Subject, Body, Date}, MessageID} | T], Terms) ->
    buildInbox(T, [ ["<tr><td>", From, "</td><td>", string:join(ToList, ", "),"</td><td>", Subject, "</td><td>" , Date, "</td><td id = 'messageid' style ='display: none;'>", MessageID, "</td></tr>"] | Terms]).

populateInbox(Browser, [], MasterBinary) -> Browser ! [{cmd, fill_div}, {id, messages}, {txt, list_to_binary(lists:reverse(MasterBinary))}];
populateInbox(Browser, [H | T], MasterBinary) ->
	populateInbox(Browser, T, lists:append(lists:reverse(H), MasterBinary)).

listToString([], String) -> String;
listToString([H], String) -> string:concat(String, io_lib:format("~p", [H]));
listToString([H | T], String) -> listToString(T, string:concat(String, io_lib:format(", ~p", [H]))).



