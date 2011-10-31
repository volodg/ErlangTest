
%Имя ноды на которой запускается сервер
-define(SRV_NODE, 'server@Mac-Pro-Vladimir').
%-define(SRV_NODE, 'server@Mac-mini-MasServer').

%Список инструментов
-define(INSTRUMENTS, [ "echo1", "echo2", "echo3", "echo4", "echo5", "echo6", "echo7", "echo8", "echo9", "echo10" ]).
%-define(INSTRUMENTS, [ "echo1", "echo2", "echo3", "echo4", "echo5" ]).

%Дата старта торгов, сейчас всегда дата запуска сервера ( игнорируется )
-define(START_DATETIME, {{2011,1,10},{12,12,12}}).

%Временной масштаб в секундах
-define(REPORT_DURATION_SEC, 5).
%дата окончания торгов определяется как START_DATETIME + REPORT_DURATION_SEC * REPORT_DURATION_NUM
-define(REPORT_DURATION_NUM, 10000).

%определяет максимально допустимое количество процессов в пуле процессов дилера
-define(DEALERS_PER_INSTRUMENT, 100).

%Структура отчета
-record(report, {
		instrument,
		open_time,
		open_price,
		close_price,
		min_price,
		max_price,
		total_amount
	}).

%Структура сделки
-record(deal, {
		instrument,
		datetime,
		price,
		amount
	}).
