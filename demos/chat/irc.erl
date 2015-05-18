-module(irc).
-export([start/0]).

start() ->
    register(irc, spawn(fun() -> start1() end)).

start1() ->
    process_flag(trap_exit, true),
    List = customio:get_lines("emojidata.txt"),
	MyList = element(2, List),
	MyMap = customio:build_emojis(MyList),
    loop([], MyMap).

loop(L, MyMap) ->
    receive
	{join, Pid, Who} ->
	    case lists:keysearch(Who,1,L) of
		false ->
			Time = clock1:current_time(),
		    L1 = L ++ [{Who,Pid}],
		    Pid ! {irc, welcome, Who},            
		    Msg = [Who, <<" joined the chat <br>">>],
		    broadcast(L1, scroll, list_to_binary(Msg)),
		    broadcast(L1, groups, list_users(L1)),
		    loop(L1, MyMap);
		{value,_} ->
		    Pid ! {irc, error, <<"Name taken">>},
		    loop(L, MyMap)
	    end;
	{leave, Who} ->
	    case lists:keysearch(Who,1,L) of
		false ->
		    loop(L, MyMap);
		{value,{Who,Pid}} ->
		    L1 = L -- [{Who,Pid}],
		    Time = clock1:current_time(),
		    Msg = [Who, <<" left the chat <br>">>],
		    broadcast(L1, scroll, list_to_binary(Msg)),
		    broadcast(L1, groups, list_users(L1)),
		    loop(L1, MyMap)
	    end;
	{broadcast, Who, Txt} ->
		Time = clock1:current_time(),
		%List = customio:get_lines("emojidata.txt"),
		%MyList = element(2, List),
		%MyMap = customio:build_emojis(MyList),
		Cut = binary_to_list(Txt),
		io:format("received ~p~n", [Cut]),
		EmojiText = emoji:match(string:to_upper(Cut), MyMap),
		io:format("converted ~p~n", [EmojiText]),
	    broadcast(L, scroll, 
		      list_to_binary([Time, "> ", Who, ": ", string:to_lower(EmojiText), "<br>"])),
	    loop(L, MyMap);
	{photoMessage, Bin, Who} ->
		Time = clock1:current_time(),
		broadcast(L, scroll, list_to_binary([Time, "> ", Who, ": ", "<img style='height:100px;' id='base64image'                 
       src='data:image/png;base64,", Bin, "' /><br>"])),
		loop(L, MyMap);
	X ->
	    io:format("irc:received:~p~n",[X]),
	    loop(L, MyMap)
    end.

broadcast(L, Tag, B) ->
    [Pid ! {irc, Tag, B} || {_,Pid} <- L].

list_users(L) ->
    L1 = [[Who,"<br>"] || {Who,_}<- L],
    list_to_binary(L1).