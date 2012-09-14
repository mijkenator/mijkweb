mijkweb
=======

tsung -f tsung_test1.xml start
cd /home/xxxx/.tsung/log/xxxxx
opt/erl15b01/lib/tsung/bin/tsung_stats.pl

TODO
========
distributed master-master mnesia
- add mnesia node
- delete mnesia node
- mysql
- memcachedb
- membase
- redis
- wired distributed test with mnesia on 2 nodes

3-7% loaded when idle, why ??? -> etop

- dialyzer

- quick sessions
- and slow (transaction) sessions
- test: choose memcached instance by function and by gen_server, autogen module
- page without sessions at all
- page with quick session
- page with quick and slow sessions
- inheritance this behaviours (parse transform, annotations, ....)
- emakefile for updates
- script wrapper for start,stop,attach ... + activation 15B01 kerl evm
- mcd library test
- riak tests
