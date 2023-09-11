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
# All nodes in the ring: [20,15,1]

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
## sever
> Pid = test:start(node2).
> register(node1, Pid).

## client
> Keys = test:keys(1000).
> Pid = {node1, 'node1@192.168.5.15'}.
> test:add(Keys, Pid).
> test:check(Keys, Pid).

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


```