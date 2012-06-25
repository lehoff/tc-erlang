#!/bin/sh
cd ~/tudo/tcc/apmc/lucia/metric/perf/../../erlang/randmat/par
erl -noshell +S $1 -s main main is_bench -s init stop
