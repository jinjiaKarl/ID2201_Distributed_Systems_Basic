#! /bin/bash

for i in {0..500..100} 
do
    if [ $i -eq 0 ]
    then
        continue
    fi
    echo "Running test $i"
    # notice the single quotes around $i
    erl -noshell -eval 'test:bench("127.0.0.1", 8080, '$i')' -s init stop
done