# A small Routing Protocol


# Main prolblems and solutionso

## get status of the node
test:get_status(r1).


## how to recover the node

when the node is down, the node will be removed from the interface. And then it will be removed from the routing table and map.

## run

### run in one region
```bash
erl -name sweden@192.168.5.15 -setcookie secret -connect_all false 
> test:start().
> test:get_state(r1).
> test:route(goteborg).
> test:stop().
```

### run in multi regions
```bash
# usa
erl -name usa@192.168.5.15 -setcookie secret -connect_all false 
> usa:start().

# china
erl -name china@192.168.5.15 -setcookie secret -connect_all false 
> china:start().

# sweden
erl -name sweden@192.168.5.15 -setcookie secret -connect_all false 
> sweden:start().


# world
erl -name world@192.168.5.15 -setcookie secret -connect_all false 
> world:start().
> world:get_status(s1, sweden).
> world:check_route(losangeles, shanghai, china).
```