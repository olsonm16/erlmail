%% Mitch Olson
%% Thursday May 14 2015

-module(user_manager).
-export([init/0, firstinit/0, create_user/2, add_friend/2, login/2, incoming_message/3, check_mail/2, get_individual_message/2, update_status/2, get_status/1, get_friends/1]).

firstinit() ->
	UsersDat = ets:new(users, [set, public]),
	ets:tab2file(UsersDat, "UsersDat"),
	MailboxDat = ets:new(mailbox, [set, public]),
	ets:tab2file(MailboxDat, "MailboxDat").

init() ->
	{ok, UsersDat} = ets:file2tab("UsersDat"),
	{ok, MailboxDat} = ets:file2tab("MailboxDat"),
	{UsersDat, MailboxDat}.

create_user(Name, Password) -> 
	{UsersDat, MailboxDat} = init(),
	case ets_contains(UsersDat, Name) of
		true -> {error, "Username already in use"};
		false -> 	UserMailboxID = make_unique_mailbox(MailboxDat, ets:new(mailbox, [set, public])),
					ets:insert(MailboxDat, {UserMailboxID, {[], 0, 0}}),
					ets:insert(UsersDat, {Name, {Password, [], offline, UserMailboxID}}),
					ets_save(UsersDat, "UsersDat"),
					ets_save(MailboxDat, "MailboxDat"),
					{success}
	end.

get_individual_message(Name, UniqueMessageID) ->
	io:format("in the user manager layer"),
	{UsersDat, MailboxDat} = init(),
	[{Name, {_CorrectPass, _FriendsList, _Status, UserMailboxID}}] = ets:lookup(UsersDat, Name),
	[{_ID, {Messages, _NewCount, _TotalCount}}] = ets:lookup(MailboxDat, UserMailboxID),
	get_one_message(Messages, UniqueMessageID).

get_one_message([], _UniqueMessageID) -> {error, "Message not found"};
get_one_message([{Message, UniqueMessageID} | _Tail], UniqueMessageID) -> {ok, Message};
get_one_message([_H |T], UniqueMessageID) -> get_one_message(T, UniqueMessageID).

make_unique_mailbox(MailboxDat, CandidateID) ->
	case ets_contains(MailboxDat, CandidateID) of
		true -> make_unique_mailbox(MailboxDat, ets:new(mailbox, [set, public]));
		false -> CandidateID
	end.

add_mailbox_message(MailboxDat, UserMailboxID, Message) ->
	[{_ID, {Messages, NewCount, TotalCount}}] = ets:lookup(MailboxDat, UserMailboxID),
	UniqueMessageID = base64:encode(crypto:strong_rand_bytes(100)),
	ets:insert(MailboxDat, {UserMailboxID, {[{Message, UniqueMessageID} | Messages], NewCount + 1, TotalCount + 1}}),
	ets_save(MailboxDat, "MailboxDat").

login(Name, Password) ->
	{UsersDat, _MailboxDat} = init(),
	case ets_contains(UsersDat, Name) of
		true -> [{Name, {CorrectPass, FriendsList, Status, UserMailboxID}}] = ets:lookup(UsersDat, Name),
				 case Password == CorrectPass of
					true -> {FriendsList, Status, UserMailboxID};
					false -> {error, "Login failed, incorrect password"}
		end;
		false -> {error, "Login failed, user does not exist"}
	end.

add_friend(Name, FriendName) ->
	{UsersDat, _MailboxDat} = init(),
	[{Name, {CorrectPass, FriendsList, Status, UserMailboxID}}] = ets:lookup(UsersDat, Name),
	ets:insert(UsersDat, {Name, {CorrectPass, [FriendName | FriendsList], Status, UserMailboxID}}).

check_mail(Name, Password) ->
	{_UsersDat, MailboxDat} = init(),
	case login(Name, Password) of
		{error, Reason} -> {error, Reason};
		{_FriendsList, _Status, UserMailboxID} -> get_messages(MailboxDat, UserMailboxID)
	end.

get_messages(MailboxDat, UserMailboxID) ->
	[{_ID, {Messages, NewCount, TotalCount}}] = ets:lookup(MailboxDat, UserMailboxID),
	{Messages, NewCount, TotalCount}.

incoming_message([H | T], Message, FailedDeliveries) ->
	{UsersDat, MailboxDat} = init(),
	case ets_contains(UsersDat, H) of
		true -> [{_Name, {_Password, _FriendsList, _Status, UserMailboxID}}] = ets:lookup(UsersDat, H),
					add_mailbox_message(MailboxDat, UserMailboxID, Message),
					incoming_message(T, Message, FailedDeliveries);
		false -> incoming_message(T, Message, [H | FailedDeliveries])
	end;

incoming_message([], _Message, FailedDeliveries) ->
	case length(FailedDeliveries) == 0 of
		true -> {success};
		false -> {error, FailedDeliveries}
	end.

ets_contains(Table, EntryKey) ->
	case ets:lookup(Table, EntryKey) == [] of
		true -> false;
		false -> true
	end.

ets_save(TableID, FileName) ->
	ets:tab2file(TableID, FileName).

update_status(Name, NewStatus) ->
	{UsersDat, _MailboxDat} = init(),
	[{Name, {CorrectPass, FriendsList, _Status, UserMailboxID}}] = ets:lookup(UsersDat, Name),
	ets:insert(UsersDat, {Name, {CorrectPass, FriendsList, NewStatus, UserMailboxID}}).

get_status(Name) ->
	{UsersDat, _MailboxDat} = init(),
	[{_Name, {_CorrectPass, _FriendsList, Status, _UserMailboxID}}] = ets:lookup(UsersDat, Name),
	Status.

get_friends(Name) ->
	{UsersDat, _MailboxDat} = init(),
	[{_Name, {_CorrectPass, FriendsList, _Status, _UserMailboxID}}] = ets:lookup(UsersDat, Name),
	FriendsList.




