-module(inbox).
-export([start/1, buildInbox/2, listToString/2]).

start(Browser) ->
	case whereis(chat_client) of
		undefined -> chat_client:start();
		PID -> io:format("~p~n", [PID])
	end,
	io:format("~p~n", [chat_client:isLoggedOn()]),
	running(Browser, chat_client:isLoggedOn()).

running(Browser, IsLoggedOn) ->
    receive
	{Browser, {struct, [getEmails]}} ->
			case IsLoggedOn of 
				true -> getEmails(Browser)
			end,
	    running(Browser, true);
	{Browser, Other} ->
		io:format("~p~n", [Other]),
		running(Browser, true)
    end.

getEmails(Browser) -> 
	io:format("Success"),
	{Messages, NewCount, TotalCount} = chat_client:check_mail(),
	Terms = [["<table align='center' width='800px' id = 'tableinbox' onload='addRowHandlers();'><thead>
          <tr><th>From</th><th>To</th><th>Subject</th><th>Date</th></tr></thead><tbody>"]],
    Bin = buildInbox(Messages, Terms),
    populateInbox(Browser, lists:reverse(Bin), []).


buildInbox([], Terms) ->  ["</tbody></table>" | Terms];
buildInbox([{{From, ToList, Subject, Body, Date}, MessageID} | T], Terms) ->
    buildInbox(T, [ ["<tr><td>", From, "</td><td>", ToList,"</td><td>", Subject, "</td><td>" , Date, "</td><td id='UniqueID' style='display: none;'>", MessageID, "</td></tr><br>"] | Terms]).

populateInbox(Browser, [], MasterBinary) -> Browser ! [{cmd, append_div}, {id, messages}, {txt, list_to_binary(lists:reverse(MasterBinary))}];
populateInbox(Browser, [H | T], MasterBinary) ->
	populateInbox(Browser, T, lists:append(lists:reverse(H), MasterBinary)).

listToString([], String) -> io_lib:format("~p", [String]);
listToString([H | T], String) -> listToString(T, string:concat(String, io_lib:format("~p", [H]))).