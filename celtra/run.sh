#!/bin/bash

rm -rf _rel
rm -f ebin/*
make
rm -f mnesia/*
erl -sname pubsub -pa ebin -pa deps/*/ebin -I include -mnesia dir '"mnesia"' -run pubsub_db mnesia_setup -s init stop -noshell && {
 make run
}
