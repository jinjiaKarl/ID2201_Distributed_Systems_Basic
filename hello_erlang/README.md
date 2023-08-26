# run in distributed mode

## first node
```
erl -name foo@192.168.5.15 -setcookie secret
> c(wait).
> Pid = spawn(wait, hello, []).
> register(wait, Pid).

```

## second node
```
erl -name bar@192.168.5.15 -setcookie secret
> net_adm:ping('foo@192.168.5.15').
> {wait, 'foo@192.168.5.15'} ! "a message from bar".

```