-module(chatv2).
-export([start/1, stop/1]).

-import(login_manager, [start/0, create_account/2, close_account/2, login/2, logout/1, online/0]).

start(Port) -> spawn(fun() ->
    % A função start() em login_manager cria um alias global chamado 'login_manager'.
    login_manager:start(),
    server(Port, login_manager)
    end).

stop(Server) -> Server ! stop.

server(Port, manager) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
    Room = spawn(fun()-> room([]) end),
    spawn(fun() -> acceptor(LSock, Room) end),
    receive stop -> ok end.

acceptor(LSock, Room) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Room) end),
    Room ! {enter, self()},
    user(Sock, Room).

room(Pids) ->
    receive
    {enter, Pid} ->
        io:format("user entered~n", []),
        room([Pid | Pids]);
    {line, Data} = Msg ->
        io:format("received ~p~n", [Data]),
        [Pid ! Msg || Pid <- Pids],
        room(Pids);
    {leave, Pid} ->
        io:format("user left~n", []),
        room(Pids -- [Pid])
end.

user(Sock, Room) ->
    receive
        {line, Data} ->
            gen_tcp:send(Sock, Data),
            user(Sock, Room);
        {tcp, _, Data} ->
            Room ! {line, Data},
            user(Sock, Room);
        {tcp_closed, _} ->
            Room ! {leave, self()};
        {tcp_error, _, _} ->
            Room ! {leave, self()}
    end.