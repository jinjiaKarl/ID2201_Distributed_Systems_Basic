#!/bin/bash

# ps -ef | grep node | grep -v 'grep'| awk '{print $2}' | xargs kill  

erl -name node1@192.168.5.15 -setcookie secret -noshell  -eval 'register(node1, test:start(node2)).' &
erl -name node2@192.168.5.15 -setcookie secret -noshell  -eval "register(node2, test:start(node2, {node1, 'node1@192.168.5.15'}))." &
erl -name node3@192.168.5.15 -setcookie secret -noshell  -eval "register(node3, test:start(node2, {node1, 'node1@192.168.5.15'}))." &
erl -name node4@192.168.5.15 -setcookie secret -noshell  -eval "register(node4, test:start(node2, {node1, 'node1@192.168.5.15'}))." &

