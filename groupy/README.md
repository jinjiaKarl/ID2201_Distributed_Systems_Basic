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
# > W1 = test:moew(3, gms2, 1000).
> W1 = test:first(1, gms2, 1000).
> test:add(2, gms2, W1, 1000) 
> test:add(3, gms2, W1, 1000) 
> test:add(3, gms2, W1, 1000) 
```