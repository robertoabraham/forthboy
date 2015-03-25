#!/bin/sh

./forth/forth ./forth/forth.fs ./forth/queue.fs ./forth/thread.fs \
    util.fs core.fs mem.fs gpu.fs cpu.fs "$1" "$2" "$3" "$4" "$5" "$6" "$7"

