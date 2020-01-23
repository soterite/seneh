seneh
=====

An OTP application

Build
-----

    $ rebar3 compile

Architecture
------------

            seneh node
                |
                V
            seneh app --------,---------------------,--------------------,
                |             |                     |                    |
                V             V                     V                    V
            seneh sup    seneh_http_listener   web_watch_handler   static watchdog.html
                |                                   |
                V       v---------------------------'
         seneh_watch worker
