
-define(SRV_NODE, 'server@Mac-Pro-Vladimir').
%-define(SRV_NODE, 'server@Mac-mini-MasServer').

-define(INSTRUMENTS, [ "echo1", "echo2", "echo3", "echo4", "echo5", "echo6", "echo7", "echo8", "echo9", "echo10" ]).
%-define(INSTRUMENTS, [ "echo1", "echo2", "echo3", "echo4", "echo5" ]).

-define(START_DATETIME, {{2011,1,10},{12,12,12}}). %ignored, now started from now.
-define(REPORT_DURATION_SEC, 5).
-define(REPORT_DURATION_NUM, 10000).

-define(DEALERS_PER_INSTRUMENT, 100).

-record(report, {
		instrument,
		open_time,
		open_price,
		close_price,
		min_price,
		max_price,
		total_amount
	}).

-record(deal, {
		instrument,
		datetime,
		price,
		amount
	}).
