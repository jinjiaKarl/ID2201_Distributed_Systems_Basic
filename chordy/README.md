# Chordy: A Distributed Hash Table

# problems and solutions

# run

start the first implementation without store
```bash

# node 1
erl -name node1@192.168.5.15 -setcookie secret
> Pid = node1:start(1).
> register(node1, Pid).
> Pid ! probe.
# All nodes in the ring: [20,15,1]  1 -> 15 -> 20 -> 1

# node 2
erl -name node2@192.168.5.15 -setcookie secret
> Pid = node1:start(20, {node1, 'node1@192.168.5.15'}).
> Pid ! probe.
# All nodes in the ring: [15,1,20]

# node3
erl -name node3@192.168.5.15 -setcookie secret
> Pid = node1:start(15, {node1, 'node1@192.168.5.15'}).
> Pid ! probe.
# All nodes in the ring: [1,20,15]

```

start the second implementation with store
```bash
# node 1
erl -name node1@192.168.5.15 -setcookie secret
> Pid = node2:start(1).
> register(node1, Pid).
> test:add(7, "hello", Pid).
> test:add(22, "hello", Pid).
> test:add(17, "hello", Pid).
> Pid ! probe.
# Node 1 endning probe, store: [{22,"hello"}]


# node 2
erl -name node2@192.168.5.15 -setcookie secret
> Pid = node2:start(20, {node1, 'node1@192.168.5.15'}).
> Pid ! probe.
# Node 20 endning probe, store: [{17,"hello"}]

# node3
erl -name node3@192.168.5.15 -setcookie secret
> Pid = node2:start(15, {node1, 'node1@192.168.5.15'}).
> Pid ! probe.
# Node 15 endning probe, store: [{7,"hello"}]
> test:lookup(7, Pid).


## using test module

> Pid = test:start(node2).
> test:start(node2, 2, Pid).
> test:add(7, "hello", Pid).
> test:lookup(7, Pid).
> Pid ! probe.

```

the second implementation of performance test
```bash
# we have eight machines and that we will use four in building the ring 
# and four in testing the performance.


# 1.only one node in the ring and let the four test machines add 1000 elements to the ring and then do a lookup of the elements
## one sever
erl -name node1@192.168.5.15 -setcookie secret
> Pid = test:start(node2).
> register(node1, Pid).

## four clients
> Keys = test:keys(1000).
> Pid = {node1, 'node1@192.168.5.15'}.
> test:add(Keys, Pid).
> test:check(Keys, Pid).

```


When there is only one server, no warnings will occur.

But wehn there is more than one server, the warning will occur.
https://erlang.org/documentation/doc-13.0-rc1/lib/kernel-8.3/doc/html/global.html

```
=WARNING REPORT==== 24-Sep-2023::18:26:02.619739 === 
'global' at node 'node2@192.168.5.15' requested disconnect from node 'node7@192.168.5.15' in order to prevent overlapping partitions
=WARNING REPORT==== 24-Sep-2023::18:26:02.654379 ===
'global' at node 'node4@192.168.5.15' requested disconnect from node 'node6@192.168.5.15' in order to prevent overlapping partitions

```

the third implementation with failure detection
```bash
# node 1
erl -name node1@192.168.5.15 -setcookie secret
> Pid = node3:start(1).
> register(node1, Pid).
> test:add(7, "hello", Pid).
> test:add(22, "hello", Pid).
> test:add(17, "hello", Pid).
> Pid ! probe.

# node 2
erl -name node2@192.168.5.15 -setcookie secret
> Pid = node3:start(20, {node1, 'node1@192.168.5.15'}).
> Pid ! probe.

# node3
erl -name node3@192.168.5.15 -setcookie secret
> Pid = node3:start(15, {node1, 'node1@192.168.5.15'}).
> Pid ! probe.
> test:lookup(7, Pid).
> # kill node3 and test:add(7, "hello", Pid). again, the node2 will take care of the data


```


improvement
* hashing of names to create unique keys for objects instead of random numbers
* consistent hash, virtual node
* implement replication
* when node joins or leaves, data migration. Now, data will be lost
