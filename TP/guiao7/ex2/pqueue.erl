-module(myqueue).
-export([create/0,enqueue/3,dequeue/1, singleton/1]).
-c(lists).

-type tree(Item) ::
	#{ prio  := int
     , elem  := Item
	 , trees := tree([Item])
	 }.

singleton(Prio, Itm) -> #{prio => Prio, elem => Itm, trees => []}.

rank(PriQueue) -> #tree.prio.

create() -> undefined.

enqueue(PriQueue, Prio, Elem) -> undefined.

dequeue(Queu) -> undefined.