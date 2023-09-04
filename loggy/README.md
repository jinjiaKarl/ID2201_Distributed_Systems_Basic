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
```bash
(usa@192.168.5.15)1> test:run_lamport(2000,1000).
log: 1 ringo {sending,{hello,27}}
log: 1 paul {sending,{hello,91}}
log: 1 george {sending,{hello,34}}
log: 2 ringo {sending,{hello,2}}
log: 2 paul {sending,{hello,8}}
log: 2 john {received,{hello,91}}
log: 3 paul {sending,{hello,52}}
log: 3 john {received,{hello,34}}
log: 4 john {received,{hello,8}}
log: 5 john {sending,{hello,77}}
log: 6 john {received,{hello,52}}
log: 6 george {received,{hello,77}}
log: 7 john {sending,{hello,63}}
log: 7 george {received,{hello,27}}
log: 8 george {sending,{hello,30}}
log: 8 paul {received,{hello,63}}
log: 9 paul {sending,{hello,47}}
log: 9 john {received,{hello,30}}

Size of Holdback Queue: 5
stop

```

## fourth try
When we use vector clock, the holdback queue is smaller than lamport clock.
```bash
(usa@192.168.5.15)5> test:run_vector(2000,1000).
log: [{george,1}] george {sending,{hello,34}}
log: [{paul,1}] paul {sending,{hello,91}}
log: [{john,1},{paul,1}] john {received,{hello,91}}
log: [{john,2},{paul,1},{george,1}] john {received,{hello,34}}
log: [{paul,2}] paul {sending,{hello,8}}
log: [{john,3},{paul,2},{george,1}] john {received,{hello,8}}
log: [{ringo,1}] ringo {sending,{hello,27}}
log: [{john,4},{paul,2},{george,1}] john {sending,{hello,77}}
log: [{george,2},{john,4},{paul,2}] george {received,{hello,77}}
log: [{george,3},{john,4},{paul,2},{ringo,1}] george {received,{hello,27}}
log: [{paul,3}] paul {sending,{hello,52}}
log: [{john,5},{paul,3},{george,1}] john {received,{hello,52}}
log: [{john,6},{paul,3},{george,1}] john {sending,{hello,63}}
log: [{paul,4},{john,6},{george,1}] paul {received,{hello,63}}
log: [{paul,5},{john,6},{george,1}] paul {sending,{hello,47}}
log: [{george,4},{john,4},{paul,2},{ringo,1}] george {sending,{hello,30}}
log: [{john,7},{paul,3},{george,4},{ringo,1}] john {received,{hello,30}}
log: [{john,8},{paul,5},{george,4},{ringo,1}] john {received,{hello,47}}
log: [{ringo,2}] ringo {sending,{hello,2}}
log: [{john,9},{paul,5},{george,4},{ringo,2}] john {received,{hello,2}}
log: [{john,10},{paul,5},{george,4},{ringo,2}] john {sending,{hello,16}}
log: [{george,5},{john,10},{paul,5},{ringo,2}] george {received,{hello,16}}

Size of Holdback Queue: 1
stop

```



## introduce lamport clock to guarantee partial order

One counter per process:
• initially set to 0
• each process increments only its clock
• sent messages are tagged with a timestamp

Receiving a message:
• set the clock to the greatest of the internal clock and the time stamp of the message (current counter = max(internal clock, timestamp of the received msg) + 1)

## introduce vector clock to guarantee total order

A vector with one counter per process:
• initially set to <0,....>
• each process increments only its index
• sent messages are tagged with a vector

Receiving a message:
• merge the internal clock and the time stamp of the message