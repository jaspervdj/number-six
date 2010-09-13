Number Six
==========

Number six is a custom IRC bot, developped for the `#ghentfpg` IRC channels.
It's still a work in progress and very alpha.

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

Down
----

Check if a website is down. Commands:

- `!down website`: Check if the website is down

EightBall
---------

Ask a question to the wise 8-ball handler. Commands:

- `!8ball question`: Ask a question

GitHub
------

Provides the latest action of a GitHub user. Commands:

- `!github username`: Prints the latest GitHub action of the given user

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

Ping
----

Replies to IRC ping's. This handler has no commands.

Quote
-----

Interface to the epic quote database. Commands:

- `!quote`: Prints a random quote
- `!quote id`: Prints the quote with the given id
- `!quote pattern`: Prints a random quote matching the given pattern
- `!lastquote`: Prints the most recently added quote
- `!addquote quote`: Adds a quote to the database
