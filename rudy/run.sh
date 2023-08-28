#! /bin/bash

# TODO: generate requests from 100 to 1000.
erl -noshell -eval 'test:bench("127.0.0.1", 8080, 100)' -s init stop