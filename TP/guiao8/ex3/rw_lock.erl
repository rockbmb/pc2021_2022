-module(rw_lock).
-export([create/0, acquire/2, release/1]).

create() ->
    spawn(fun released/0).

acquire(Pid, Mode) when Mode == read; Mode == write ->
    Pid ! {Mode, self()},
    receive
        acquired -> true
    end.

release(Pid) ->
    Pid ! {release, self()}.

destroy(Lock) -> Lock ! stop.

released() ->
    receive
        {read, Pid}  ->
            Pid ! acquired,
            reading([Pid]);
        {write, Pid} ->
            Pid ! acquired,
            writing(Pid)
    end.

reading([]) ->
    released();
reading(Readers) ->
    receive
        {read, Pid}    ->
            Pid ! acquired,
            reading([Pid | Readers]);
        {release, Pid} ->
            reading(Readers -- [Pid]);
        {write, Pid}   ->
            reading(Readers, Pid)
    end.

reading([], Writer) ->
    Writer ! acquire,
    writing(Writer);
reading(Readers, Writer) ->
    receive
        {release, Pid} -> reading(Readers -- [Pid], Writer)
    end.

writing(Pid) ->
    receive
        % Q: Has to be the same as the Pid in argument?
        % A: Yes, Erlang has few occurences of shadowing.
        {release, Pid} -> released()
    end.