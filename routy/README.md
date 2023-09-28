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

# 有usa

Name: beijing
 N: 9
History: [{stockholm,5},{newyork,1},{guangzhou,0},{shanghai,1},{beijing,8}]
Gateways: [{newyork,#Ref<0.2105182708.886308866.169374>,{u1,'usa@192.168.5.15'}},{shanghai,#Ref<0.2105182708.886308866.169262>,{c2,'china@192.168.5.15'}}]
Table: [{gothenburg,newyork},{guangzhou,shanghai},{losangeles,newyork},{beijing,newyork},{stockholm,newyork},{shanghai,shanghai},{newyork,newyork}]
Map: [{stockholm,[newyork,gothenburg]},{beijing,[newyork,shanghai]},{shanghai,[guangzhou,beijing]},{newyork,[stockholm,beijing,losangeles]},{guangzhou,[shanghai]}]

# 删除usa 发送update
Name: beijing
 N: 9
History: [{stockholm,5},{newyork,1},{guangzhou,0},{shanghai,1},{beijing,8}]
Gateways: [{shanghai,#Ref<0.2105182708.886308866.169262>,{c2,'china@192.168.5.15'}}]
Table: [{gothenburg,shanghai},{losangeles,shanghai},{stockholm,shanghai},{newyork,shanghai},{beijing,shanghai},{guangzhou,shanghai},{shanghai,shanghai}]
Map: [{stockholm,[newyork,gothenburg]},{beijing,[newyork,shanghai]},{shanghai,[guangzhou,beijing]},{newyork,[stockholm,beijing,losangeles]},{guangzhou,[shanghai]}]

# 删除usa 发送broadcast，通知其他节点beijing和newwork不连接了
Name: beijing
 N: 10
History: [{stockholm,5},{newyork,1},{guangzhou,0},{shanghai,1},{beijing,9}]
Gateways: [{shanghai,#Ref<0.2105182708.886308866.169262>,{c2,'china@192.168.5.15'}}]
Table: [{gothenburg,shanghai},{losangeles,shanghai},{stockholm,shanghai},{newyork,shanghai},{beijing,shanghai},{guangzhou,shanghai},{shanghai,shanghai}]
Map: [{beijing,[shanghai]},{stockholm,[newyork,gothenburg]},{shanghai,[guangzhou,beijing]},{newyork,[stockholm,beijing,losangeles]},{guangzhou,[shanghai]}]

# 删除usa 发送broadcast，再发送update
Name: beijing
 N: 10
History: [{stockholm,5},{newyork,1},{guangzhou,0},{shanghai,1},{beijing,9}]
Gateways: [{shanghai,#Ref<0.2105182708.886308866.169262>,{c2,'china@192.168.5.15'}}]
Table: [{beijing,shanghai},{guangzhou,shanghai},{shanghai,shanghai}]
Map: [{beijing,[shanghai]},{stockholm,[newyork,gothenburg]},{shanghai,[guangzhou,beijing]},{newyork,[stockholm,beijing,losangeles]},{guangzhou,[shanghai]}]


Kill usa node; need to broadcast(c1, s1) and update all nodes to get the latest routing table.
when usa is added agagin, need to broadcast the update again.

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