#!/bin/sh

# Handle generating the makefile
autoconf
./configure
make && make test
