#!/bin/sh

gforth sdl.fs util.fs core.fs mem.fs gpu.fs cpu.fs main.fs \
  "$1" "$2" "$3" "$4" "$5" "$6" "$7"

