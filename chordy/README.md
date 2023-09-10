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