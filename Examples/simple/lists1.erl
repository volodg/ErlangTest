-module(lists1).
-export([reverse/1]).

reverse(L) ->
	reverse(L, []).

reverse([H|T], L) ->
	reverse(T, [H|L]);

reverse([], L) ->
	L.