
-define(SRV_NODE, 'server@Mac-Pro-Vladimir').
-define(SRV_NAME, bn_server).

-define(INSTRUMENTS, [ "echo1", "echo2", "echo3", "echo4", "echo5", "echo6", "echo7", "echo8", "echo9", "echo10" ]).

-define(REPORT_DURATION_SEC, 60).

%-record(deal_rec, { client_pid, { instrument, time, price, amount } } ).
