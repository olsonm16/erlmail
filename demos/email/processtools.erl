-module(processtools).
-export([stringActivePIDs/0, clientPID/1, activePIDs/0, findProcessFromString/1]).

stringActivePIDs() -> lists:map(fun(A) -> lists:nth(1, A) end, lists:map(fun(A) -> io_lib:format("~p", [A]) end, erlang:processes())).

activePIDs() -> erlang:processes().

filterPIDString([], _PIDString, _Count) -> [];
filterPIDString([PIDString | _T], PIDString, Count) -> lists:nth(Count, activePIDs());
filterPIDString([_H | T], PIDString, Count) -> filterPIDString(T, PIDString, Count + 1).

findProcessFromString(PIDString) ->
	filterPIDString(stringActivePIDs(), PIDString, 1).

clientPID(ClientString) ->
	{ok, Tokens, _} = erl_scan:string(ClientString),
	filterTokens(Tokens, []).

filterTokens([], ListOfInts) -> lists:reverse(ListOfInts);
filterTokens([{integer, _, Integer} | T], ListOfInts) -> filterTokens(T, [Integer | ListOfInts]);
filterTokens([_H | T], ListOfInts) -> filterTokens(T, ListOfInts).


