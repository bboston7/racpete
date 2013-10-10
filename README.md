racpete
=======
racpete is (will be) a super cool irc bot written entirely in Racket!  Since
development has just begun, there is much work todo.  Consider tackling
something from the high priority list below and submitting a pull request when
done.  Alternatively, if you want to work on something else, that's also cool.

What to do
----------
### High Priority ###
- Basic irc networking support
    - CONNECT
    - JOIN
    - PONG
    - PART
    - QUIT
    - PRIVMSG (Terrible name, this is how you send a message to a CHANNEL as
      well)
- Logging / log parsing

### Medium Priority ###
- Multichannel support
- Classic IRC bot fuctions
    - .q
    - tell me about
    - Get link title
    - Anything else you think may be fun

### Low Priority ###
- Multiserver support

Potentially Helpful Resources
-----------------------------
- [Super basic python IRC bot](http://travismccrea.com/2010/02/write-a-basic-python-irc-bot/)
- [IRC Spec](http://tools.ietf.org/html/rfc2812)
- [Racket networking docs](http://docs.racket-lang.org/net/)
