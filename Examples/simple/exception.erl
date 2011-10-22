-module(exception).
-export([demo/1]).

foo(1) ->
	hello;
foo(2) ->
	throw({myerror, abc});
foo(3) ->
	tuple_to_list(a);
foo(4) ->
	exit({myExit, 222}).

demo(X) ->
	case catch foo(X) of
		{myerror, Args} ->
			{user_error, Args};
		{'EXIT', What} ->
			{caught_error, What};
		Other ->
			Other
	end.
