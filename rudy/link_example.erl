-module(link_example).
-compile(export_all).

myproc() ->
    timer:sleep(1000),
    exit("I'm dead").

spawn_myproc() ->
    register(link, spawn_link(fun() -> myproc() end)),
    % 将普通进程变成系统进程，就可以捕获到子进程的退出消息
    % process_flag(trap_exit, true),
    % receive 
    %     Msg ->
    %         Msg   % Output: {'EXIT',<0.109.0>,"I'm dead"}
    % end.

