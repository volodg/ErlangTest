-module(sort1).
-export([reverse_sort/1, sort/1]).

reverse_sort(L) ->
	lists1:reverse(sort(L)).

sort(L) ->
	lists:sort(L).