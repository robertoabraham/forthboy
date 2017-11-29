\ Forth GB (c) 2015 Braden Shepherdson
\ Version 1
\ Startup and Gameboy core

\ The Gameboy has several memory areas, which I will create now.
\ The main thing not created here is the ROM, which is created during loading.

8 KB CARRAY vram \ 8000 - 9FFF
8 KB CARRAY sram \ A000 - BFFF
8 KB CARRAY wram \ C000 - DFFF

160 CARRAY oam   \ FE00 - FE9F
128 CARRAY hram  \ FF80 - FFFE

\ Registers and words for accessing them
\ A, B, C, D, E, F, H and L
HERE
12 ALLOT

   DUP CONSTANT reg-a
1+ DUP CONSTANT reg-f
1+ DUP CONSTANT reg-b
1+ DUP CONSTANT reg-c
1+ DUP CONSTANT reg-d
1+ DUP CONSTANT reg-e
1+ DUP CONSTANT reg-h
1+ DUP CONSTANT reg-l
1+ DUP CONSTANT reg-sp
2+     CONSTANT reg-pc

: a@ reg-a C@ ; : a! reg-a C! ;
: f@ reg-f C@ ; : f! reg-f C! ;
: b@ reg-b C@ ; : b! reg-b C! ;
: c@ reg-c C@ ; : c! reg-c C! ;
: d@ reg-d C@ ; : d! reg-d C! ;
: e@ reg-e C@ ; : e! reg-e C! ;
: h@ reg-h C@ ; : h! reg-h C! ;
: l@ reg-l C@ ; : l! reg-l C! ;

: af@ a@ 8 LSHIFT f@ OR ;
: af! DUP 255 AND f!   8 RSHIFT 255 AND a! ;
: bc@ b@ 8 LSHIFT c@ OR ;
: bc! DUP 255 AND c!   8 RSHIFT 255 AND b! ;
: de@ d@ 8 LSHIFT e@ OR ;
: de! DUP 255 AND e!   8 RSHIFT 255 AND d! ;
: hl@ h@ 8 LSHIFT l@ OR ;
: hl! DUP 255 AND l!   8 RSHIFT 255 AND h! ;

\ I can't implement SP- and PC-related words without being able to read and
\ write from the real memory, with mapping and all.
\ The plan: shift the address by 12, leaving only the most significant hex digit
\ and then index with that into the following arrays.

HEX
1FFF CONSTANT 8K-mask
3FFF CONSTANT 16K-mask
4000 CONSTANT rom-bank-size
DECIMAL

\ Defining the IO registers, with their rules.
128 CARRAY io-ports
128 CARRAY io-port-write-masks

: io-mask-init 128 0 DO 255 I io-port-write-masks C! LOOP ;
io-mask-init

HEX
40 CONSTANT io-LCDC
41 CONSTANT io-STAT
F8 41 io-port-write-masks C!  \ Bottom 3 bits are not writeable.
42 CONSTANT io-SCY
43 CONSTANT io-SCX
44 CONSTANT io-LY             \ Writing to this causes a reset.
0 44 io-port-write-masks C!   \ Which is exactly what a mask of 0 will do!
45 CONSTANT io-LYC
4A CONSTANT io-WY
4B CONSTANT io-WY

47 CONSTANT io-BGP
48 CONSTANT io-OBP0
49 CONSTANT io-OBP1

46 CONSTANT io-DMA            \ Technically write-only, no indication of what
                              \ gets read from here. *shrug*

0  CONSTANT io-JOYP
30 0 io-port-write-masks C!

4  CONSTANT io-DIV
0  4 io-port-write-masks C!   \ Writing to DIV resets it to 0.
5  CONSTANT io-TIMA
6  CONSTANT io-TMA
7  CONSTANT io-TAC
7  7 io-port-write-masks C!   \ Only the bottom 3 bits are tracked.

FF CONSTANT io-IE
0F CONSTANT io-IF


1  CONSTANT int-mask-vblank
2  CONSTANT int-mask-stat
4  CONSTANT int-mask-timer
8  CONSTANT int-mask-serial
10 CONSTANT int-mask-joypad

40 CONSTANT int-vector-vblank
48 CONSTANT int-vector-stat
50 CONSTANT int-vector-timer
58 CONSTANT int-vector-serial
60 CONSTANT int-vector-joypad

 1 CONSTANT lcdc-BG-enable
 2 CONSTANT lcdc-OBJ-enable
 4 CONSTANT lcdc-OBJ-size
 8 CONSTANT lcdc-BG-tile-map
10 CONSTANT lcdc-BG-tile-data
20 CONSTANT lcdc-window-enable
40 CONSTANT lcdc-window-tile-map
80 CONSTANT lcdc-enable

 4 CONSTANT stat-coincidence-flag
 8 CONSTANT stat-int-hblank
10 CONSTANT stat-int-vblank
20 CONSTANT stat-int-oam
40 CONSTANT stat-int-coincidence
DECIMAL


VARIABLE rom-bank
1 rom-bank !

: rom-0-read ( gb-addr -- value ) rom C@ ;
: rom-banked-read ( gb-addr -- value )
    16K-mask AND               ( offset )
    rom-bank-size rom-bank @ * ( offset bank-base )
    + rom C@
  ;

HEX
: vram-read ( gb-addr -- value ) 8K-mask AND   vram C@ ;
: sram-read ( gb-addr -- value ) 8K-mask AND   sram C@ ;
: wram-read ( gb-addr -- value ) 8K-mask AND   wram C@ ;
: oam-read  ( offset  -- value ) oam C@ ;
: io-read   ( offset  -- value ) io-ports C@ ;
: high-read ( gb-addr -- value )
    DUP FF80 >= IF 7F AND hram C@ EXIT THEN
    DUP FF00 >= IF 7F AND io-read EXIT THEN
    DUP FE00 >= IF FF AND oam-read  EXIT THEN
    wram-read
  ;
DECIMAL

16 ARRAY read-handlers
: read-handler-init ( )
    ' rom-0-read  DUP 0 read-handlers !
                  DUP 1 read-handlers !
                  DUP 2 read-handlers !
                      3 read-handlers !
    ' rom-banked-read DUP 4 read-handlers !
                      DUP 5 read-handlers !
                      DUP 6 read-handlers !
                          7 read-handlers !
    ' vram-read DUP 8  read-handlers ! 9  read-handlers !
    ' sram-read DUP 10 read-handlers ! 11 read-handlers !
    ' wram-read DUP 12 read-handlers ! 13 read-handlers !
                    14 read-handlers ! \ ECHO of lower WRAM
    ' high-read 15 read-handlers !
  ;
read-handler-init

: gb@ ( gb-addr -- value ) DUP 12 RSHIFT read-handlers @ EXECUTE ;


\ What happens on writes depends on the Memory Bank Controller in the cart.
\ The following are for ROM-only, unbanked ROMs like Tetris.
\ TODO Implement whatever MBCs I care about, and a way to toggle at load-time.

16 ARRAY write-handlers

HEX

: non-write ( value gb-addr -- ) 2DROP ;
' non-write
DUP 0 write-handlers !
DUP 1 write-handlers !
DUP 2 write-handlers !
DUP 3 write-handlers !
DUP 4 write-handlers !
DUP 5 write-handlers !
DUP 6 write-handlers !
    7 write-handlers !

: vram-write ( value gb-addr -- ) 1FFF AND vram C! ;
' vram-write DUP 8 write-handlers ! 9 write-handlers !

: sram-write ( value gb-addr -- ) 1FFF AND sram C! ;
' sram-write DUP A write-handlers ! B write-handlers !

: wram-write ( value gb-addr -- ) 1FFF AND wram C! ;
' wram-write DUP C write-handlers ! DUP D write-handlers ! E write-handlers !

: oam-write  ( value offset  -- ) oam C! ;
: io-write   ( value offset  -- )
    DUP io-port-write-masks C@ ( value offset mask )
    ROT AND                    ( offset masked-value )
    SWAP io-ports C!
  ;
: high-write ( value gb-addr -- )
    DUP FF80 >= IF 7F AND hram C!   EXIT THEN
    DUP FF00 >= IF 7F AND io-write  EXIT THEN
    DUP FE00 >= IF FF AND oam-write EXIT THEN
    wram-write
  ;
' high-write F write-handlers !
DECIMAL

: gb! ( value gb-addr -- ) DUP 12 RSHIFT write-handlers @ EXECUTE ;

\ The stack is decrement-before, increment-after, same as ARM.
: push ( val -- )
    reg-sp H@ 1 - ( val sp' )
    2DUP gb!      ( val sp' )
    reg-sp H! DROP
  ;

: pop ( -- val )
    reg-sp H@    ( sp )
    DUP gb@      ( sp value )
    SWAP 1 +     ( value sp' )
    reg-sp H!
  ;

: pushH ( val -- ) DUP 8 RSHIFT FF AND push FF AND push ;
: popH  ( -- val ) pop pop 8 LSHIFT OR ;

: sp@ ( -- val ) reg-sp H@ ;
: sp! ( val -- ) reg-sp H! ;

\ Fetches the byte currently at PC.
: pc@ ( -- val ) reg-pc H@ gb@ ;
\ Bumps PC by a given delta.
: pc+ ( delta -- ) reg-pc H@ + reg-pc H! ;
\ Bumps by one.
: pc++ ( -- ) 1 pc+ ;
\ Read a byte and bump PC
: pc@+ ( -- value ) pc@ pc++ ;

: pc@H+ ( -- value ) pc@+ pc@+ ( lo hi ) 8 LSHIFT OR ;

: pc! ( value -- ) reg-pc H! ;

: (nn)@ ( -- value ) pc@H+ gb@ ;
: (nn)! ( -- value ) pc@H+ gb! ;

: (bc)@ bc@ gb@ ;
: (bc)! bc@ gb! ;
: (de)@ de@ gb@ ;
: (de)! de@ gb! ;
: (hl)@ hl@ gb@ ;
: (hl)! hl@ gb! ;

