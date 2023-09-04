# a logical time logger


# Problems and Solutions

# run
## first try
```bash
# the order is wrong, we receive the message before sending it
(usa@192.168.5.15)3> test:run(2000,1000).
log: na john {received,{hello,91}}
log: na john {received,{hello,34}}
log: na george {sending,{hello,34}}
log: na paul {sending,{hello,91}}
log: na john {received,{hello,8}}
log: na george {received,{hello,77}}
log: na george {received,{hello,27}}
log: na paul {sending,{hello,8}}
log: na john {sending,{hello,77}}
log: na john {received,{hello,52}}
log: na ringo {sending,{hello,27}}
log: na paul {sending,{hello,52}}
log: na paul {received,{hello,63}}
log: na john {sending,{hello,63}}
log: na john {received,{hello,30}}
log: na john {received,{hello,47}}
log: na john {received,{hello,2}}
log: na paul {sending,{hello,47}}
log: na george {sending,{hello,30}}
log: na george {received,{hello,16}}
log: na ringo {sending,{hello,2}}
log: na john {sending,{hello,16}}
log: na ringo {received,{hello,3}}
stop

# the order is correct
(usa@192.168.5.15)4> test:run(2000,0).
log: na paul {sending,{hello,91}}
log: na john {received,{hello,91}}
log: na george {sending,{hello,34}}
log: na john {received,{hello,34}}
log: na george {sending,{hello,32}}
log: na john {received,{hello,32}}
log: na paul {sending,{hello,83}}
log: na john {received,{hello,83}}
log: na paul {sending,{hello,99}}
log: na ringo {received,{hello,99}}
log: na john {sending,{hello,36}}
log: na paul {received,{hello,36}}
log: na ringo {sending,{hello,21}}
log: na john {received,{hello,21}}
log: na george {sending,{hello,30}}
log: na john {received,{hello,30}}
log: na john {sending,{hello,79}}
log: na paul {received,{hello,79}}
log: na john {sending,{hello,18}}
log: na george {received,{hello,18}}
log: na ringo {sending,{hello,2}}
log: na john {received,{hello,2}}
log: na paul {sending,{hello,98}}
log: na ringo {received,{hello,98}}
log: na john {sending,{hello,35}}
log: na paul {received,{hello,35}}
stop

```

## second try
When we only set the time, the order is still out of order.
```bash
(usa@192.168.5.15)3> test:run_lamport_no_q(2000,1000).
log: 1 john {received,{hello,91}}
log: 1 john {received,{hello,34}}
log: 1 george {sending,{hello,34}}
log: 1 paul {sending,{hello,91}}
log: 2 john {received,{hello,8}}
log: 5 george {received,{hello,77}}
log: 1 george {received,{hello,27}}
log: 2 paul {sending,{hello,8}}
log: 5 john {sending,{hello,77}}
log: 3 john {received,{hello,52}}
log: 1 ringo {sending,{hello,27}}
log: 3 paul {sending,{hello,52}}
log: 7 paul {received,{hello,63}}
log: 7 john {sending,{hello,63}}
log: 8 john {received,{hello,30}}
log: 9 john {received,{hello,47}}
log: 2 john {received,{hello,2}}
log: 9 paul {sending,{hello,47}}
log: 8 george {sending,{hello,30}}
log: 12 george {received,{hello,16}}
log: 2 ringo {sending,{hello,2}}
log: 12 john {sending,{hello,16}}
log: 10 ringo {received,{hello,3}}
stop
```

## third try
loggy collect all log messages and sort them by time, if message is safe, print it out.
```
(usa@192.168.5.15)1> test:run_lamport(2000,1000).
log: 1 ringo {sending,{hello,27}}
log: 1 george {received,{hello,27}}
log: 1 paul {sending,{hello,91}}
log: 1 george {sending,{hello,34}}
log: 1 john {received,{hello,34}}
log: 1 john {received,{hello,91}}
log: 2 ringo {sending,{hello,2}}
log: 2 john {received,{hello,2}}
log: 2 paul {sending,{hello,8}}
log: 2 john {received,{hello,8}}
log: 3 paul {sending,{hello,52}}
log: 3 john {received,{hello,52}}
log: 5 john {sending,{hello,77}}
log: 5 george {received,{hello,77}}
log: 7 john {sending,{hello,63}}
log: 7 paul {received,{hello,63}}
log: 8 george {sending,{hello,30}}
log: 8 john {received,{hello,30}}
log: 9 paul {sending,{hello,47}}
log: 9 john {received,{hello,47}}
stop

```



## introduce lamport clock to guarantee partial order

One counter per process:
• initially set to 0
• each process increments only its clock
• sent messages are tagged with a timestamp

Receiving a message:
• set the clock to the greatest of the internal clock and the time stamp of the message (current counter = max(internal clock, timestamp of the received msg) + 1)
