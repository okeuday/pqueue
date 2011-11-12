Erlang Priority Queue Implementation
====================================

This priority queue implementation implements a subset of the stdlib Erlang queue interface as seen in the implementation used by both [Riak and RabbitMQ](https://github.com/basho/riak_core/blob/master/src/priority_queue.erl).
However, this implementation uses tuple storage instead of a sorted list for
[faster "in" operations at the slight expense of "out" operations](http://okeuday.livejournal.com/19187.html), with [the benchmark here](http://github.com/okeuday/erlbench).

Author
------

Michael Truog (mjtruog [at] gmail (dot) com)

Thanks
------

Jesper Louis andersen     PropEr integration and testing
Ulf Wiger                 Suggestions and Insight

License
-------

BSD

