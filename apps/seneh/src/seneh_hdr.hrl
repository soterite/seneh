-define(LOG_FILE, "/tmp/seneh_log").
-define(USER_CONFIG_FILE, lists:concat([os:getenv("HOME"), "/.seneh/configuration.xml"])).
-define(DEFAULT_CONFIG_FILE, "/apps/seneh/examples/configuration.xml").

-define(LOGGER_BUS, logger_bus).
-define(HTTP_PORT, 8080).

-record(log_message, {format = "~p",
                      data = []}).

-record(file, {name,
               path}).

-record(file_table, {content = []}).

-record(file_watcher, {name,
                       period,
                       files = #file_table{}}).

-record(process, {name,
                  start_cmd,
                  activity_indicator}).

-record(process_table, {content = [],
                        ps_CMD = "ps -xo cmd"}).

-record(process_watcher, {name,
                          period,
                          processes = #process_table{}}).
