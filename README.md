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


TODO
----

- MR-1 - Introduce xml/json configuration for seneh_watch worker
    - xml/json configuration
    - configuration_watcher (?) for updating and restarting changes in configuration
-
