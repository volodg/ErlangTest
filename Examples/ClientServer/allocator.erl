-module(allocator).
-export([start/1,allocate/0,free/1]).

% The server.
server(Free, Allocated) ->
	receive
		{From,alloc} ->
			allocate(Free, Allocated, From);
		{From,{free,R}} ->
			free(Free, Allocated, From, R)
	end.

start(Resources) ->
	Pid = spawn(allocator, server, [Resources,[]]),
	register(resource_alloc, Pid).

% The interface functions.
allocate() ->
	request(alloc).

free(Resource) ->
	request({free,Resource}).
	
request(Request) ->
	resource_alloc ! {self(),Request},
	receive
		{resource_alloc,Reply} ->
			Reply
	end.