automaton
=========

Defines applicative instances for different types of automata: finite state machines, pushdown automata, and queue automata. I use the library for writing parsers: the queue automata applicative will specifically parse context-free grammars using a breadth-first search strategy, and can also be used to simulate running computations in parallel.