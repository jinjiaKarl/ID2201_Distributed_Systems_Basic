#!/bin/bash

# ps -ef | grep node | grep -v 'grep'| awk '{print $2}' | xargs kill  

if [ $# -eq 0 ]; then
    echo "No arguments provided"
    exit 1
fi

if [ $1 == "stop" ]; then
    ps -ef | grep node | grep -v 'grep'| awk '{print $2}' | xargs kill  
    exit 1
fi

if [ $1 == "one" ]; then
    echo "Starting one server node"
    erl -name node1@192.168.5.15 -setcookie secret -noshell  -eval 'register(node1, test:start(node2)).'
fi

if [ $1 == "four" ]; then
    echo "Starting four server nodes"
    erl -name node1@192.168.5.15 -setcookie secret -connect_all false -noshell  -eval 'register(node1, test:start(node2)).' &
    sleep 2
    erl -name node2@192.168.5.15 -setcookie secret -connect_all false -noshell  -eval "register(node2, test:start(node2, {node1, 'node1@192.168.5.15'}))." &
    sleep 2
    erl -name node3@192.168.5.15 -setcookie secret -connect_all false -noshell  -eval "register(node3, test:start(node2, {node1, 'node1@192.168.5.15'}))." &
    sleep 2
    erl -name node4@192.168.5.15 -setcookie secret -connect_all false  -noshell  -eval "register(node4, test:start(node2, {node1, 'node1@192.168.5.15'}))." &
    echo "Starting four server nodes background done"
fi

