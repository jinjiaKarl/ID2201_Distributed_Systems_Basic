# A Group Memebership Service



# Problems and Solutions


Why do we need application process and group process?


# architecture

The application layer is unaware whether its group process acts as a leader or a slave.

leader group process 
* broadcast msg to all slaves and its application process
* handle join/leave and broadcast new view to all slaves and its application process

slave group process
* forward msg to its application process and leader group process

# run

## gms1
```bash
1> W1 = test:first(1, gms1,1000).
Master 1 started
<0.84.0>
2> test:add(2, gms1, W1, 1000).
Slave 2 started
<0.91.0>
Group 1 multicasting {view,[<0.85.0>,<0.92.0>],[<0.84.0>,<0.91.0>]}
Group 1 multicasting {msg,{state_request,#Ref<0.3769713491.1692139521.90227>}}
Group 1 multicasting {msg,{state,#Ref<0.3769713491.1692139521.90227>,
                                 {133,137,130}}}
Group 1 stopped
```

## gms2
```bash
# start 4 nodes, and kill the leader node one by one
1> W1 = test:more(4, gms2, 1000).
Master 1 started
Slave 2 started
Slave 3 started
Slave 4 started
<0.84.0>
Group 1 stopped
Slave 2: election
Slave 3: election
Slave 4: election
Slave 2: I am the new leader
Group 2 stopped
Slave 3: election
Slave 4: election
Slave 3: I am the new leader
Group 3 stopped
Slave 4: election
Slave 4: I am the new leader
Group 4 stopped
2> 

# > W1 = test:first(1, gms2, 1000).
# > test:add(2, gms2, W1, 1000).
# > test:add(3, gms2, W1, 1000). 
# > test:add(3, gms2, W1, 1000). 
# > test:add(4, gms2, W1, 1000). 
```
## gms2_crash
```bash
# when the leader is dead, the slaves's state is out of sync
# the process of broadcast is not atomic, if the leader is dead, the change msgs cannot forward to all slaves
1> W1 = test:more(10, gms2_crash, 1000).

# slow down the speed of chaneg color, set the arghh 100, extend the sleep to 10000
1> W1 = test:more(10, gms2_crash, 10000).
```

## gms3
```bash
# 1> W1 = test:more(5, gms3, 1000).
> W1 = test:first(1, gms3, 1000).
> W2 = test:add(2, gms3, W1, 1000).
> W3 = test:add(3, gms3, W1, 1000).
> W4 = test:add(4, gms3, W1, 1000).
> W5 = test:add(5, gms3, W1, 1000).

> W6 = test:add(5, gms3, W2, 1000).
```

## gms4
In erlang, messages are delivered in FIFO order, not that they actually do arrive. So the message could be lost, we need ACK mechanism to ensure the message is delivered.

```bash
1> W1 = test:more(5, gms4, 1000).

```