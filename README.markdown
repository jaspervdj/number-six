Number Six
==========

[![Build Status](https://secure.travis-ci.org/jaspervdj/number-six.png?branch=master)](http://travis-ci.org/jaspervdj/number-six)

Number six is a custom IRC bot, developed for student IRC channels at UGent.

Compiling & Running
===================

Begin by installing the library:

    cabal configure
    cabal build
    cabal install

Them, you can get started by copying `Sample.hs` and starting from there.
Configure it, then compile & run using:

    `ghc --make Sample.hs`
    `./Sample`

Handlers
========

Handlers are plugins which react to IRC messages. Here, we give a short listing
of the available Handlers, together with the commands they provide.

Binary
------

Binary conversions. Commands:

- `!bin decimal`: Convert a number number to binary notation
- `!unbin binary`: Convert a binary number to decimal notation
- `!hex decimal`: Convert a decimal number to hex notation
- `!unhex`: Convert a hex number to decimal notation

Bomb
----

A cool IRC game.

- `!bomb nick`: Attach a bomb to the specified user. This bomb will kick the
  user in a certain amount of time (a countdown will be visible).
- `!pass`: Passes the bomb to the user who placed the bomb. This command can
  only be issued by the current person owning them bomb.
- `!pass nick`: Passes the bomb to the specified user

EightBall
---------

Ask a question to the wise 8-ball handler. Commands:

- `!8ball question`: Ask a question

GitHub
------

Provides the latest action of a GitHub user. Commands:

- `!github username`: Prints the latest GitHub action of the given user

Gods
----

Check who the gods are on this server, and manage them. Commands:

- `!gods`: Show the gods on this server
- `!addgod password`: This adds the sender as a god. Obviously, you should run
  this in a private query.
- `!removegod`: Removes you as a god.
- `!removegod pattern`: Removes all gods which match the given pattern.

Google
------

Searches the web using Google. Commands:

- `!google query`, `!g query`: Searches google for query and returns the first
  link found

HackerNews
----------

Prints the title and link to an item on Hacker News. Commands:

- `!hn number`: Prints the item which is currently on the given position on
  Hacker News.

Hello
-----

Test handler, simply greets a user. Commands:

- `!hello`: Greet the user

Help
----

Provides information. Commands:

- `!help`: Print help information

Http
----

Sends an HTTP GET request to a server and returns the status line, useful if you
want to check if a web service is down for everyone or just you. Commands:

- `!http uri`: Check the given URI

Identify
--------

System handler which sets the nickname and the real name of the bot. This
handler has no commands.

Imdb
----

Lookup a movie on IMDB. Commands:

- `!imdb title`: Lookup the given movie.

Insult
------

Insult a user. Commands:

- `!insult user`: Insults the given user.

Join
----

Have the bot join channels. The bot will automatically join the channels
specified in the the configuration file. When the bot is kicked out of a
channel, this handler will try to rejoin the channel. Commands:

- `!join channel`: Join the specified channel

LastFm
------

Check what track a user last listened to on last.fm. Commands:

- `!lastfm username`: Print the track the given user last listened to.

NickServ
--------

Authenticates the bot with NickServ. This handler has no commands.

NowPlaying
----------

Show the currently playing track on a radio station. Commands:

- `!nowplaying station`: Print the currently playing track on the given radio
  station

Supported radio stations:

- [rgrfm](http://www.rgrfm.be/), [stubru](http://www.stubru.be/) and
  [urgent](http://urgent.fm/)

Op
--

Give channel operator privileges. Commands:

- `!op nick`: Given channel operator privileges to the specified user
- `!deop nick`: Take away channel operator privileges from the specified user

Pick
----

Pick a random item from a list. Commands:

- `!pick item1 item2 item3 ...`, `!who ...`: Print a random item from the list.

Ping
----

Replies to IRC ping's. This handler has no commands.

Reddit
------

Print a random item from a subreddit. Commands:

- `!reddit subreddit`: Prints a random item from the given subreddit
- `!reddit subreddit number`: Print the given item from the given subreddit
- `!reddit`: Print a random item from the front page
- `!reddit number`: Print the given item from the front page

Remind
------

Set reminders for yourself. Commands:

- `!remind some note`: Store a note
- `!remind`: Collect stored notes

Resto
-----

Print the menu of today in the Ghent University resto's, see also
<https://github.com/blackskad/Resto-menu>

- `!resto`: Print today's menu
- `!resto day`: `day` can be: `morgen`, `tomorrow`, `overmorgen`,
  `day after tomorrow`

Rr
--

Challenge a user to a game of russian roulette. Commands:

- `!rr username`: Challenge the specified user

Quote
-----

Interface to the epic quote database. Commands:

- `!quote`: Prints a random quote
- `!quote id`: Prints the quote with the given id
- `!quote pattern`: Prints a random quote matching the given pattern
- `!lastquote`: Prints the most recently added quote
- `!addquote quote`: Adds a quote to the database

Say
---

Make the bot say arbitrary text. God is required. Commands:

- `!say channel text`: Say the given text in the given channel

Seen
----

Check when a user was last seen in the channel. Commands:

- `!seen username`: Check when the given user was last seen

Shorten
-------

Shortens a URL using bit.ly. Commands:

- `!shorten url`: Shorten the given URL

Sup
---

When the user greets the bot with `sup botname`, the bot will respond with
`sup username`.

Tell
----

Send a message to a user who is currently away. Commands:

- `!tell username message`: Tell the message to the given user when he becomes
  active.

Topic
-----

Set the channel topic. Only gods may give this command:

- `!topic message`: Set the channel topic

TryHaskell
----------

Bindings to the haskell evaluation service of tryhaskell.org. Commands:

- `> expression`, `!haskell expression`: Evaluate the given expression

TryRuby
-------

Bindings to the ruby evaluation service of tryruby.org. Commands:

- `!ruby expression`: Evaluate the given expression

Tumblr
------

Get either a random tumble from the specified user of his last tumble.

- `!tumblr last username`: Print the slug and the URL for the last tumble user
  posted
- `!tumblr username`: Print the slug and the URL from a random tumble made by
  the user, currently only taking the last 50 into account

Twitter
-------

Handler to get twitters by id or user. Commands:

- `!twitter username`: Prints the last tweet of the given user
- `!twitter id`: Prints the tweet with the given id

UrbanDictionary
---------------

Lookup an expression on urbandictionary.com. Commands:

- `!urban term`: Looks up the given term on urban dictionary

Voice
-----

When people join the channel, the bot will give them voice (`+v`). This can also
be set manually, altough this requires god privileges:

- `!voice nick`: Give a user voice
- `!devoice nick`, `!stfu nick`: Take voice from a user

Weather
-------

How is the fucking weather? Commands:

- `!weather`: Prints the weather in Ghent
- `!weather city`: Prints the weather in the given city

Wikipedia
---------

Lookup a term on wikipedia. Commands:

- `!w term, !wik term, !wiki term`: Looks up the given term on wikipedia

YouTube
-------

Lookup a video on youtube.com. Commands:

- `!youtube term, !y term`: Looks for videos related to the given term on youtube
