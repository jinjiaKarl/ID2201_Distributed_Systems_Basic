#!/bin/bash

# if result.txt exists, delete it
if [ -f results.txt ]; then
    rm results.txt
fi
erl -name node5@192.168.5.15 -setcookie secret -connect_all false  -noshell  -eval "test:benchmark(1000, {node1, 'node1@192.168.5.15'})." -s init stop >> results.txt & 
erl -name node6@192.168.5.15 -setcookie secret -connect_all false  -noshell  -eval "test:benchmark(1000, {node1, 'node1@192.168.5.15'})." -s init stop >> results.txt &
erl -name node7@192.168.5.15 -setcookie secret -connect_all false  -noshell  -eval "test:benchmark(1000, {node1, 'node1@192.168.5.15'})." -s init stop >> results.txt &
erl -name node8@192.168.5.15 -setcookie secret -connect_all false  -noshell  -eval "test:benchmark(1000, {node1, 'node1@192.168.5.15'})." -s init stop >> results.txt &