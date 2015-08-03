# Erlmail : An Erlang email service

This project was created for my Distributed Systems class in Spring 2015 at W&L. 

The project is complete, and is (sometimes) running at http://nick.network/email/create_account.html. Usually it is not running, as I use that site for a variety of purposes. Ping me if you'd like a demo.

[Full overview presentation PDF here](https://drive.google.com/file/d/0B2EiUMTOjw1rVFFuUzM1V3o2eXc)

## Overview

* Erlmail is written entirely in Erlang, except for the HTML/CSS/JavaScript for site functionality.
* The backend includes features for user and message storage, login, saved photo background, message delivery, message deletion, and account deletion.
* The email server is stored on a TCP server, and in the browser users create an instance of the email client, which connects to the server.
* We modeled it after basic email functionality, and ended up with a pretty clean model thanks to Erlang.

## User Model

* We created two databases using the built in ETS storage:
  * UsersDat
  * MailboxDat
* Objects in UsersDat
  * Key: Username
  * Value: {Password, Saved Photo, UniqueMailboxID}
* Objects in MailboxDat
  * Key: UniqueMailboxID
  * Value: {List of Messages, Count}

* Pretty much all Erlang tuples, except for Messages List.

## Message Model

* Message: {MessageContent, UniqueMessageID}
* MessageContent: {From, ToList, Subject, Body, Date}
* UniqueMessageID: base64:encode(crypto:strong_rand_bytes(100))
* A message is delivered recursively through the ToList list of recipients, and a Failure list is kept to inform the user of recipients who could not be reached.
* The message is stored in the User’s Mailbox if the User is reached.
* The UniqueMessageID is used in the Inbox to get a selected message’s body.

## Misc Features

* Clients retain the ability to customize their interface with a background photo, which is saved as a base64 text file and rendered as such in HTML.
* Clients can at any time clear their inbox or destroy their own account. Changes are made permanently to the ETS tables and saved at the time of the request.
* Clients may refresh their Inbox manually if they wish, as the chat client running in the background does not change its login state unless a user logs out.

##Frontend to Backend Hierarchy

Best demonstrated thru an example:

![alt text](http://i.imgur.com/gIbTpJs.jpg "Example")

###Sources

Joe Armstrong's [ezwebframe](https://github.com/joearms/ezwebframe)






   
