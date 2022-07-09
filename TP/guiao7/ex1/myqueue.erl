-module(myqueue).
-export([create/0,enqueue/2,dequeue/1]).
-c(lists).

% -type queue(Item):: [Item].


create() -> {[],[]}.

% enqueue(Queue :: queue, Item) = lists:append([Item],Queue).

enqueue({F,R}, I) -> {[I|F],R}.
dequeue({[],[]}) -> empty;

dequeue({[H|T],[]}) -> {{[],lists:reverse(T)},H};
dequeue({L,[H|T]}) -> {{L,T},H}.