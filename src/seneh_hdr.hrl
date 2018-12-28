-define(LOG_FILE, "/tmp/seneh_log").
-define(LOGGER_BUS, logger_bus).

-record(process, {name,
                  start_cmd,
                  activity_indicator}).
-record(process_table, {content = [],
                        ps_CMD = "ps -xo cmd"}).
