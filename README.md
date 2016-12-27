racpete
=======
racpete is the raddest, baddest irc bot ever written entirely in Racket!

How do I use racpete?
-----------------------------------
- racpete is primarily tested on Linux machines running racket v6.1 or higher,
  so a good first step is to install the latest racket runtime.
- Clone the repo onto the machine you want the bot to run on.
- In ```/src``` edit ```config.rkt``` to contain appropriate values, like
  channel name, and pete's nick.
- Choose a password which you will use when performing restricted operations such
  as the ```.die``` command that kills the bot.  To set this password, overwrite
  the ```.racpete.pass``` file in ```/src``` with the password's SHA-512 hash.
  An easy way to do this on Linux is to change into the ```/src``` directory and
  issue the command ```echo -n "password" | sha512sum | awk '{ print $1 }' >
  .racpete.pass``` where password is your password.
- You're done!  To start pete, just execute ```./run.bash```.

Want to contribute?
-------------------
Look at the [issue tracker](https://github.com/bboston7/racpete/issues) for
specific ideas of features to implement or bugs to fix.

If you are scared, confused or lonely, feel free to contact
[bboston7](https://github.com/bboston7) or
[johnislarry](https://github.com/johnislarry) for general purpose guidance or
otherwise sage wisdom.

### Potentially Helpful Resources ###
- [Super basic python IRC bot](http://travismccrea.com/2010/02/write-a-basic-python-irc-bot/)
- [IRC Spec](http://tools.ietf.org/html/rfc2812)
- [Racket networking docs](http://docs.racket-lang.org/net/)
- [Simple scheme IRC bot](http://pastebin.com/RQUMFbC9)
