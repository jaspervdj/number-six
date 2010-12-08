Number Six
==========

Number six is a custom IRC bot, developped for the `#ghentfpg` IRC channels.
It's still a work in progress and very alpha.

Handlers
========

Handlers are plugins which react to IRC messages. Here, we give a short listing
of the available Handlers, together with the commands they provide.

AutoVoice
---------

When people join the channel, the bot will give them voice (`+v`). This handler
has no commands.

Binary
------

Binary conversions. Commands:

- `!bin decimal`: Convert a number number to binary notation
- `!unbin binary`: Convert a binary number to decimal notation
- `!hex decimal`: Convert a decimal number to hex notation
- `!unhex`: Convert a hex number to decimal notation

Cubits
------

Give or take cubits from a user. God is required for giving or taking cubits.
Commands:

- `!cubits`: Print the number of cubits of the sender
- `!cubits nick`: Print the number of cubits of the specified user
- `!cubits nick n`: Give `n` (possibly negative) cubits to the specified user

Down
----

Check if a website is down. Commands:

- `!down url`: Check if the website is down

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
- `!removegod nick`: Removes the given user as god.

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

Identify
--------

System handler which sets the nickname and the real name of the bot. This
handler has no commands.

Kick
----

System handler which rejoins a channel after being kicked. This handler has no
commands.

LastFm
------

Check what track a user last listened to on last.fm. Commands:

- `!lastfm username`: Print the track the given user last listened to.

NowPlaying
----------

Show the currently playing track on a radio station. Commands:

- `!nowplaying station`: Print the currently playing track on the given radio
  station

Supported radio stations:

- `stubru`

Op
--

Give channel operator privileges. Commands:

- `!op nick`: Given channel operator privileges to the specified user
- `!deop nick`: Take away channel operator privileges from the specified user

Ping
----

Replies to IRC ping's. This handler has no commands.

Reddit
------

Print a random item from a subreddit. Commands:

- `!reddit name`: Prints a random item from the given subreddit

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

Slap
----

Shows the bot's superiority to a user. Commands:

- `!slap nick`: Slaps the specified user around a bit.

Tell
----

Send a message to a user who is currently away. Commands:

- `!tell username message`: Tell the message to the given user when he becomes
  active.

Title
-----

Prints the title of a web page. Commands:

- `!title url`: Print the title of the given webpage

TryHaskell
----------

Bindings to the haskell evaluation service of tryhaskell.org. Commands:

- `> expression`, `!haskell expression`: Evaluate the given expression

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
