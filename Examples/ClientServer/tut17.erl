-module(tut17).

-export([start_ping/1, start_pong/0,  ping/2, pong/0]).

ping(0, Pong_Node) ->
    {pong, Pong_Node} ! finished, 
    io:format("Пинг завершил работу~n", []);

ping(N, Pong_Node) ->
    {pong, Pong_Node} ! {ping, self()}, 
    receive
        pong ->
            io:format("Пинг получил понг~n", [])
    end, 
    ping(N - 1, Pong_Node).

pong() ->
    receive
        finished ->
            io:format("Понг завершил работу~n", []);
        {ping, Ping_PID} ->
            io:format("Понг получил пинг~n", []), 
            Ping_PID ! pong, 
            pong()
    end.

start_pong() ->
    register(pong, spawn(tut17, pong, [])).

start_ping(Pong_Node) ->
    spawn(tut17, ping, [3, Pong_Node]).

%start server
%macserver$ erl -sname ping
%tut17:start_pong().

%start client
%tut17:start_ping('ping@Mac-mini-MasServer').
