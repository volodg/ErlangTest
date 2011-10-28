
-define(SRV_NODE, 'server@Mac-Pro-Vladimir').
%-define(SRV_NODE, 'server@Mac-mini-MasServer').

%ETODO ganarate instruments by Number of it
-define(INSTRUMENTS, [ "echo1", "echo2", "echo3", "echo4", "echo5", "echo6", "echo7", "echo8", "echo9", "echo10" ]).

-define(REPORT_DURATION_SEC, 5).

-record(report, {
		instrument,
		open_time,
		open_price,
		close_price,
		min_proce,
		max_price,
		total_amount
	}).

-record(deal, {
		instrument,
		datetime,
		price,
		amount
	}).
