# a small web server

## main problems and solutions

how to kill one process from another?
```

erl -name foo@192.168.5.15 -setcookie secret -eval 'server:start(8080)'  -noshell


# registered() to find all registered processes, but could not find the pid of the rudy process? how to deal with it?
# erl -name bar@192.168.5.15 -setcookie secret -eval 'server:stop()' -noshell

# use rpc library to call the function in the remote node
erl -name bar@192.168.5.15 -setcookie secret -eval 'rpc:call('foo@192.168.5.15', server, stop, [])' -noshell


```


## benchmark

environment:
* host: Darwin Kernel Version 22.6.0
* guest: 6.2.0-27-generic #28-Ubuntu, ip: 192.168.105.2/24 
* delay from guest to host: round-trip min/avg/max/stddev = 1.191/2.763/6.126/1.967 ms (ping 192.158.105.2 -c 5)

1. How many requests per second can we serve? 
```bash
make start
make bench

# output
test: 21.54826434117798 requests per second
```


2. Is our artificial delay significant, or does it disappear in the parsing overhead? 

Yes, the artificial delay matters, the parsing overhead is same for every request. Becasue our server now is single-threaded, and the delay is added to each request. The request is handled one by one.

3. What happens if you run the benchmarks on several machines simultaneously? 
* Increased Load: Simultaneously running benchmarks from multiple machines will increase the load on the server.
* Increased Response Times: As the server becomes busier, response times can increase due to the accumulation of queued requests and the need to process them sequentially.
* Queueing and Backlog: As the server processes requests sequentially, if the incoming request rate exceeds its processing rate, a backlog of queued requests can form. 
* Resource Utilization: The server might not be able to utilize multiple CPU cores efficiently.

## improvements

### muti-threaded

requests from 100 to 1000.

#### one request per thread

```bash
# start server
make start_multi

# terminal 1
make bench_multi

# terminal 2
make bench_multi

```

#### thread poll

```bash
# start server
make start_poll

# terminal 1
make bench_poll

# terminal 2
make bench_poll
```

#### benchmark using k6
```bash
k6 run k6_main.js
```