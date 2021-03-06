-module(myelib).
-export([my_ip/0, rpc/2]).

my_ip() ->
    {ok, L} = inet:getifaddrs(),
    case lists:keysearch("enp2s0f0", 1, L) of
    	false -> {value, {_,L1}} = lists:keysearch("lo:1", 1, L);
    	{_value, {_, _L1}} -> {value, {_,L1}} = lists:keysearch("enp2s0f0", 1, L)
    end,
    get_ip4(L1).

get_ip4([{addr,{A,B,C,D}}|_]) -> {A,B,C,D};
get_ip4([_|T])                -> get_ip4(T);
get_ip4([])                   -> [].

rpc(Pid, Query) ->
    Self = self(),
    Pid ! {Self, Query},
    receive
	{Pid, Reply} ->
	    Reply
    end.
