# IRC Command Reference #
This document contains an incomplete reference for the irc commands that racpete supports

## Simple Commands ##
Simple commands produce the same static output every time.
* `.test` -- responds with "caught .test"
* `.ycombinator` -- produces a graphical representation of the ycombinator from programming language theory
* `.version` -- Prints the current version number of racpete

## Network Query Commands ##
* `.g <query>` -- Performs a Google search with `<query>`
* `.w <query>` -- Searches Wikipedia for `<query>`
* `.yt <query>` -- Searches YouTube for `<query>`
* `.bash` -- Returns a random quote from bash.org
* `.cc <query>` -- Returns the price in USD of a cryptocurrency from coincap.io

## Log Based Commands ##
These commands use racpete's channel logs.  They'll improve as racpete idles in the channel more.
* `.q` -- Returns a random quote from the channel
* `.link me` -- Returns a random link from the channel
* `tell me about <query>` -- Returns a quote from the channel containing `<query>`
* `.rx <regex>` -- Returns a random quote matching the regular expression `<regex>`
* `what has <nick> said` -- Returns a random quote from `<nick>`
* `what would <nick> say` -- Constructs a response in the style of `<nick>`.  If `<nick>` is the bot's nick, it will construct a response in the style of the entire channel log.

## Karma Commands ##
racpete also contains a karma tracking system.
* `.karma <item>` -- Returns the karma of `<item>`
* `.karma` -- Prints a karma leaderboard
* `.rkarma` -- Returns the karma of a random item
* `<item>++` or `++<item>` -- Increments the karma of `<item>` by one
* `<item>--` or `--<item>` -- Decrements the karma of `<item>` by one

## Conversions ##
* `.morse <string>` -- Translates `<string>` to Morse code
* `<morse code>` -- Morse code is automatically translated back to alphanumeric characters
* `.c <number> <from> <to>` -- Converts `<number>` from base `<from>` to base `<to>`
