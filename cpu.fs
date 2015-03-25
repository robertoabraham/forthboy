\ Forth GB (c) 2015 Braden Shepherdson
\ Version 1
\ Gameboy CPU emulator

\ RAM and ROM blocks are defined in core.fs. This file contains the opcodes
\ and interpreter loop.

\ First some flags definitions.
\ flag-X@ reads the flag X as a Forth boolean
\ flag-Xs sets the flag X
\ flag-Xc clears the flag X
\ flag-X! sets the flag according to its argument: nonzero argument - set flag.
: flag-Z@ ( -- ? ) f@ 128 AND 0<> ;
: flag-Zs ( -- )   f@ 128 OR f! ;
: flag-Zc ( -- )   f@ 128 INVERT AND f! ;
: flag-Z! ( n -- ) 0> IF flag-Zs ELSE flag-Zc THEN ;
: flag-N@ ( -- ? ) f@  64 AND 0<> ;
: flag-Ns ( -- )   f@  64 OR f! ;
: flag-Nc ( -- )   f@  64 INVERT AND f! ;
: flag-N! ( n -- ) 0> IF flag-Ns ELSE flag-Nc THEN ;
: flag-H@ ( -- ? ) f@  32 AND 0<> ;
: flag-Hs ( -- )   f@  32 OR f! ;
: flag-Hc ( -- )   f@  32 INVERT AND f! ;
: flag-H! ( n -- ) 0> IF flag-Hs ELSE flag-Hc THEN ;
: flag-C@ ( -- ? ) f@  16 AND 0<> ;
: flag-Cs ( -- )   f@  16 OR f! ;
: flag-Cc ( -- )   f@  16 INVERT AND f! ;
: flag-C! ( n -- ) 0> IF flag-Cs ELSE flag-Cc THEN ;


\ Then some global variables
VARIABLE cpu-halted   0 cpu-halted !
VARIABLE gpu-halted   0 gpu-halted !

\ This ranges from -2 to +2. When nonzero, it moves one step towards 0 on each
\ instruction. When it moves from -1 to 0, interrupts are disabled. When it
\ moves from 1 to 0, interrupts are enabled.
VARIABLE interrupt-future   0 interrupt-future !
VARIABLE interrupt-enable   FALSE interrupt-enable !

VARIABLE delay-cycles       0 delay-cycles !

\ This array holds all the opcodes, which are defined below.
256 ARRAY opcodes
256 ARRAY cycles

: mk-op-LD ( "word" lhs rhs opcode cycles -- )
    SWAP                          ( lhs rhs cycles opcode )
    DOCOL CREATE LATEST @ (>CODE) ( lhs rhs opcode cycles xt )
    DUP HERE !
    ' DOCOL ,
    OVER opcodes !                ( lhs rhs cycles opcode )
    cycles !
    , ,
    ' EXIT ,
  ;

HEX
' b! ' pc@+ 06 8 mk-op-LD op-LD-B,n
' c! ' pc@+ 0E 8 mk-op-LD op-LD-C,n
' d! ' pc@+ 16 8 mk-op-LD op-LD-D,n
' e! ' pc@+ 1E 8 mk-op-LD op-LD-E,n
' h! ' pc@+ 26 8 mk-op-LD op-LD-H,n
' l! ' pc@+ 2E 8 mk-op-LD op-LD-L,n

' a! ' b@    78 4 mk-op-LD op-LD-A,B
' a! ' c@    79 4 mk-op-LD op-LD-A,C
' a! ' d@    7A 4 mk-op-LD op-LD-A,D
' a! ' e@    7B 4 mk-op-LD op-LD-A,E
' a! ' h@    7C 4 mk-op-LD op-LD-A,H
' a! ' l@    7D 4 mk-op-LD op-LD-A,L
' a! ' (hl)@ 7E 8 mk-op-LD op-LD-A,(hl)

' b! ' b@    40 4 mk-op-LD op-LD-B,B
' b! ' c@    41 4 mk-op-LD op-LD-B,C
' b! ' d@    42 4 mk-op-LD op-LD-B,D
' b! ' e@    43 4 mk-op-LD op-LD-B,E
' b! ' h@    44 4 mk-op-LD op-LD-B,H
' b! ' l@    45 4 mk-op-LD op-LD-B,L
' b! ' (hl)@ 46 8 mk-op-LD op-LD-B,(hl)

' c! ' b@    48 4 mk-op-LD op-LD-C,B
' c! ' c@    49 4 mk-op-LD op-LD-C,C
' c! ' d@    4A 4 mk-op-LD op-LD-C,D
' c! ' e@    4B 4 mk-op-LD op-LD-C,E
' c! ' h@    4C 4 mk-op-LD op-LD-C,H
' c! ' l@    4D 4 mk-op-LD op-LD-C,L
' c! ' (hl)@ 4E 8 mk-op-LD op-LD-C,(hl)

' d! ' b@    50 4 mk-op-LD op-LD-D,B
' d! ' c@    51 4 mk-op-LD op-LD-D,C
' d! ' d@    52 4 mk-op-LD op-LD-D,D
' d! ' e@    53 4 mk-op-LD op-LD-D,E
' d! ' h@    54 4 mk-op-LD op-LD-D,H
' d! ' l@    55 4 mk-op-LD op-LD-D,L
' d! ' (hl)@ 56 8 mk-op-LD op-LD-D,(hl)

' e! ' b@    58 4 mk-op-LD op-LD-E,B
' e! ' c@    59 4 mk-op-LD op-LD-E,C
' e! ' d@    5A 4 mk-op-LD op-LD-E,D
' e! ' e@    5B 4 mk-op-LD op-LD-E,E
' e! ' h@    5C 4 mk-op-LD op-LD-E,H
' e! ' l@    5D 4 mk-op-LD op-LD-E,L
' e! ' (hl)@ 5E 8 mk-op-LD op-LD-E,(hl)

' h! ' b@    60 4 mk-op-LD op-LD-H,B
' h! ' c@    61 4 mk-op-LD op-LD-H,C
' h! ' d@    62 4 mk-op-LD op-LD-H,D
' h! ' e@    63 4 mk-op-LD op-LD-H,E
' h! ' h@    64 4 mk-op-LD op-LD-H,H
' h! ' l@    65 4 mk-op-LD op-LD-H,L
' h! ' (hl)@ 66 8 mk-op-LD op-LD-H,(hl)

' l! ' b@    68 4 mk-op-LD op-LD-L,B
' l! ' c@    69 4 mk-op-LD op-LD-L,C
' l! ' d@    6A 4 mk-op-LD op-LD-L,D
' l! ' e@    6B 4 mk-op-LD op-LD-L,E
' l! ' h@    6C 4 mk-op-LD op-LD-L,H
' l! ' l@    6D 4 mk-op-LD op-LD-L,L
' l! ' (hl)@ 6E 8 mk-op-LD op-LD-L,(hl)

' (hl)! ' b@   70 8 mk-op-LD op-LD-(hl),B
' (hl)! ' c@   71 8 mk-op-LD op-LD-(hl),C
' (hl)! ' d@   72 8 mk-op-LD op-LD-(hl),D
' (hl)! ' e@   73 8 mk-op-LD op-LD-(hl),E
' (hl)! ' h@   74 8 mk-op-LD op-LD-(hl),H
' (hl)! ' l@   75 8 mk-op-LD op-LD-(hl),L
' (hl)! ' pc@+ 36 C mk-op-LD op-LD-(hl),n

' a! ' a@    7F 4 mk-op-LD op-LD-A,A
' a! ' b@    78 4 mk-op-LD op-LD-A,B
' a! ' c@    79 4 mk-op-LD op-LD-A,C
' a! ' d@    7A 4 mk-op-LD op-LD-A,D
' a! ' e@    7B 4 mk-op-LD op-LD-A,E
' a! ' h@    7C 4 mk-op-LD op-LD-A,H
' a! ' l@    7D 4 mk-op-LD op-LD-A,L

' a! ' (bc)@ 0A 8  mk-op-LD op-LD-A,(bc)
' a! ' (de)@ 1A 8  mk-op-LD op-LD-A,(de)
' a! ' (hl)@ 7E 8  mk-op-LD op-LD-A,(hl)
' a! ' (nn)@ FA 10 mk-op-LD op-LD-A,(nn)
' a! ' pc@+  3E 8  mk-op-LD op-LD-A,n

' b!    ' a@ 47  4 mk-op-LD op-LD-B,A
' c!    ' a@ 4F  4 mk-op-LD op-LD-C,A
' d!    ' a@ 57  4 mk-op-LD op-LD-D,A
' e!    ' a@ 5F  4 mk-op-LD op-LD-E,A
' h!    ' a@ 67  4 mk-op-LD op-LD-H,A
' l!    ' a@ 6F  4 mk-op-LD op-LD-L,A
' (bc)! ' a@ 02  8 mk-op-LD op-LD-(bc),A
' (de)! ' a@ 12  8 mk-op-LD op-LD-(de),A
' (hl)! ' a@ 77  8 mk-op-LD op-LD-(hl),A
' (nn)! ' a@ EA 16 mk-op-LD op-LD-(nn),A


: op-LD-A,(C) c@ FF00 + gb@ a! ;
' op-LD-A,(C) F2 opcodes ! 8 F2 cycles !
: op-LD-(C),A a@ c@ FF00 + gb! ;
' op-LD-(C),A E2 opcodes ! 8 E2 cycles !

: op-LD-A,(HLD) (hl)@ a!   hl@ 1- hl! ;
' op-LD-A,(HLD) 3A opcodes! 8 3A cycles !
: op-LD-(HLD),A a@ (hl)!   hl@ 1- hl! ;
' op-LD-(HLD),A 32 opcodes! 8 32 cycles !

: op-LD-A,(HLI) (hl)@ a!   hl@ 1+ hl! ;
' op-LD-A,(HLI) 2A opcodes! 8 2A cycles !
: op-LD-(HLI),A a@ (hl)!   hl@ 1+ hl! ;
' op-LD-(HLI),A 22 opcodes! 8 22 cycles !


: op-LD-(HI+n),A a@ pc@+ FF00 + gb! ; ' op-LD-(HI+n),A E0 opcodes! C E0 cycles !
: op-LD-A,(HI+n) pc@+ FF00 + gb@ a! ; ' op-LD-A,(HI+n) F0 opcodes! C F0 cycles !

' bc! ' pc@H+ 01 C mk-op-LD op-LD-BC,nn
' de! ' pc@H+ 11 C mk-op-LD op-LD-DE,nn
' hl! ' pc@H+ 21 C mk-op-LD op-LD-HL,nn
' sp! ' pc@H+ 31 C mk-op-LD op-LD-SP,nn

' sp! ' hl@   F9 8 mk-op-LD op-LD-SP,HL

: op-LDHL-SP,n
    pc@+ sp@  ( a b )
    2DUP F AND SWAP F AND + ( a b half )
    10  >= IF flag-Hs ELSE flag-Hc THEN
    + DUP hl!  \ XXX Is this storing the address or the value at it?
    100 >= IF flag-Cs ELSE flag-Cc THEN
  ;
' op-LDHL-SP,n F8 opcodes ! C F8 cycles !

' (nn)! ' sp@ 08 14 mk-op-LD op-LD-(nn),SP


\ PUSH - XXX are these little-endian? I'm not sure which way around these are
\ stored. I went with storing them as a 16-bit word little-endian, ie. with the
\ less significant portion at the lower address.
: op-PUSH-af a@ push f@ push ; ' op-PUSH-af F5 opcodes ! 10 F5 cycles !
: op-PUSH-bc b@ push c@ push ; ' op-PUSH-bc C5 opcodes ! 10 C5 cycles !
: op-PUSH-de d@ push e@ push ; ' op-PUSH-de D5 opcodes ! 10 D5 cycles !
: op-PUSH-hl h@ push l@ push ; ' op-PUSH-hl E5 opcodes ! 10 E5 cycles !

\ POP - XXX As above.
: op-POP-af pop f! pop a! ; ' op-POP-af F1 opcodes ! C F1 cycles !
: op-POP-bc pop c! pop b! ; ' op-POP-bc C1 opcodes ! C C1 cycles !
: op-POP-de pop e! pop d! ; ' op-POP-de D1 opcodes ! C D1 cycles !
: op-POP-hl pop l! pop h! ; ' op-POP-hl E1 opcodes ! C E1 cycles !



: add-helper-set-H ( lhs rhs -- )
    F AND SWAP F AND +
    F0 AND flag-H!
  ;
: add-helper-main ( lhs rhs -- sum )
    FF AND SWAP FF AND +
    DUP FF00 AND flag-C!
    FF AND
  ;

\ ADD A,n
: mk-op-ADD ( "word" rhs opcode cycles -- )
    SWAP                          ( rhs cycles opcode )
    DOCOL CREATE LATEST @ (>CODE) ( rhs cycles opcode xt )
    DUP HERE !
    ' DOCOL ,
    OVER opcodes !                ( rhs cycles opcode )
    cycles !
    ' a@ ,
    ,                           ( C: ) ( X: lhs rhs )
    \ Add the lower halves to find H
    ' 2DUP , ' add-helper-set-H ,
    \ Add the upper halves to find C and the sum
    ' add-helper-main ,     ( X: sum ) \ Now C has been set appropriately.
    ' flag-Nc , \ Always clears N.
    ' DUP , ' 0<> , ' flag-Z! , ( X: sum ) \ Sets Z properly. Have to invert.
    ' a! ,
    ' EXIT ,
  ;

' a@    87 4 mk-op-ADD op-ADD-A,A
' b@    80 4 mk-op-ADD op-ADD-A,B
' c@    81 4 mk-op-ADD op-ADD-A,C
' d@    82 4 mk-op-ADD op-ADD-A,D
' e@    83 4 mk-op-ADD op-ADD-A,E
' h@    84 4 mk-op-ADD op-ADD-A,H
' l@    85 4 mk-op-ADD op-ADD-A,L
' (hl)@ 86 8 mk-op-ADD op-ADD-A,(hl)
' pc@+  C6 8 mk-op-ADD op-ADD-A,n


: adc-helper-set-H ( lhs rhs -- )
    flag-C@ IF 1 ELSE 0 THEN -ROT ( carry lhs rhs )
    F AND SWAP F AND + +
    F0 AND flag-H!
  ;
: adc-helper-main ( lhs rhs -- sum )
    flag-C@ IF 1 ELSE 0 THEN -ROT ( carry lhs rhs )
    FF AND SWAP FF AND + +
    DUP FF00 AND flag-C!
    FF AND
  ;

\ ADC A,n
: mk-op-ADC ( "word" rhs opcode cycles -- )
    SWAP                          ( rhs cycles opcode )
    DOCOL CREATE LATEST @ (>CODE) ( rhs cycles opcode xt )
    DUP HERE !
    ' DOCOL ,
    OVER opcodes !                ( rhs cycles opcode )
    cycles !
    ' a@ ,
    ,                           ( C: ) ( X: lhs rhs )
    \ Add the lower halves to find H
    ' 2DUP , ' adc-helper-set-H ,
    \ Add the upper halves to find C and the sum
    ' adc-helper-main ,     ( X: sum ) \ Now C has been set appropriately.
    ' flag-Nc , \ Always clears N.
    ' DUP , ' 0<> , ' flag-Z! , ( X: sum ) \ Sets Z properly. Have to invert.
    ' a! ,
    ' EXIT ,
  ;

' a@    8F 4 mk-op-ADC op-ADC-A,A
' b@    88 4 mk-op-ADC op-ADC-A,B
' c@    89 4 mk-op-ADC op-ADC-A,C
' d@    8A 4 mk-op-ADC op-ADC-A,D
' e@    8B 4 mk-op-ADC op-ADC-A,E
' h@    8C 4 mk-op-ADC op-ADC-A,H
' l@    8D 4 mk-op-ADC op-ADC-A,L
' (hl)@ 8E 8 mk-op-ADC op-ADC-A,(hl)
' pc@+  CE 8 mk-op-ADC op-ADC-A,n



\ SUB - Subtracts arg from A, adjusting flags etc.
: sub-helper-H ( lhs rhs -- )
    F AND SWAP       ( rhs' lhs )
    F AND 10 OR SWAP ( lhs' rhs' )
    - F > flag-H!    \ Flag is set when there's no borrow.
  ;
: sub-helper-C ( lhs rhs -- res )
    FF AND SWAP
    FF AND 100 OR SWAP ( lhs' rhs' )
    - DUP FF > flag-C!     \ Flag is set when there's no brrow.
    FF AND
  ;

: mk-op-SUB ( "word" rhs opcode cycles -- )
    SWAP                          ( rhs cycles opcode )
    DOCOL CREATE LATEST @ (>CODE) ( rhs cycles opcode xt )
    DUP HERE !
    ' DOCOL ,
    OVER opcodes !                ( rhs cycles opcode )
    cycles !
    ' a@ ,
    ,                           ( C: ) ( X: lhs rhs )
    \ Add the lower halves to find H
    ' 2DUP , ' sub-helper-H ,
    \ Add the upper halves to find C and the sum
    ' sub-helper-C ,     ( X: sum ) \ Now C has been set appropriately.
    ' flag-Ns , \ Always sets N.
    ' DUP , ' 0<> , ' flag-Z! , ( X: sum ) \ Sets Z properly. Have to invert.
    ' a! ,
    ' EXIT ,
  ;

' a@    97 4 mk-op-SUB op-SUB-A,A
' b@    90 4 mk-op-SUB op-SUB-A,B
' c@    91 4 mk-op-SUB op-SUB-A,C
' d@    92 4 mk-op-SUB op-SUB-A,D
' e@    93 4 mk-op-SUB op-SUB-A,E
' h@    94 4 mk-op-SUB op-SUB-A,H
' l@    95 4 mk-op-SUB op-SUB-A,L
' (hl)@ 96 8 mk-op-SUB op-SUB-A,(hl)
' pc@+  D6 8 mk-op-SUB op-SUB-A,n


\ SBC - Subtracts arg from A, adjusting flags etc. with carry.
: sbc-helper-H ( lhs rhs -- )
    flag-C@ IF 1 + THEN
    F AND SWAP       ( rhs' lhs )
    F AND 10 OR SWAP ( lhs' rhs' )
    - F > flag-H!    \ Flag is set when there's no borrow.
  ;
: sbc-helper-C ( lhs rhs -- )
    flag-C@ IF 1 + THEN
    FF AND SWAP
    FF AND 100 OR SWAP ( lhs' rhs' )
    - DUP FF > flag-C!     \ Flag is set when there's no brrow.
    FF AND
  ;

: mk-op-SBC ( "word" rhs opcode cycles -- )
    SWAP                          ( rhs cycles opcode )
    DOCOL CREATE LATEST @ (>CODE) ( rhs cycles opcode xt )
    DUP HERE !
    ' DOCOL ,
    OVER opcodes !                ( rhs cycles opcode )
    cycles !
    ' a@ ,
    ,                           ( C: ) ( X: lhs rhs )
    \ Add the lower halves to find H
    ' 2DUP , ' sbc-helper-H ,
    \ Add the upper halves to find C and the sum
    ' sbc-helper-C ,     ( X: sum ) \ Now C has been set appropriately.
    ' flag-Ns , \ Always sets N.
    ' DUP , ' 0<> , ' flag-Z! , ( X: sum ) \ Sets Z properly. Have to invert.
    ' a! ,
    ' EXIT ,
  ;

' a@    9F 4 mk-op-SBC op-SBC-A,A
' b@    98 4 mk-op-SBC op-SBC-A,B
' c@    99 4 mk-op-SBC op-SBC-A,C
' d@    9A 4 mk-op-SBC op-SBC-A,D
' e@    9B 4 mk-op-SBC op-SBC-A,E
' h@    9C 4 mk-op-SBC op-SBC-A,H
' l@    9D 4 mk-op-SBC op-SBC-A,L
' (hl)@ 9E 8 mk-op-SBC op-SBC-A,(hl)
' pc@+  DE 8 mk-op-SBC op-SBC-A,n

: update-arrays ( opcode cycles -- )
    OVER             ( opcode cycles opcode )
    LATEST @ (>CODE) ( opcode cycles opcode xt )
    SWAP opcodes !   ( opcode cycles )
    SWAP cycles !    ( )
  ;

: op-AND-A    a@ a@    AND DUP NOT flag-Z! a! flag-Nc flag-Hs flag-Cc ; A7 4 update-arrays
: op-AND-B    a@ b@    AND DUP NOT flag-Z! a! flag-Nc flag-Hs flag-Cc ; A0 4 update-arrays
: op-AND-C    a@ c@    AND DUP NOT flag-Z! a! flag-Nc flag-Hs flag-Cc ; A1 4 update-arrays
: op-AND-D    a@ d@    AND DUP NOT flag-Z! a! flag-Nc flag-Hs flag-Cc ; A2 4 update-arrays
: op-AND-E    a@ e@    AND DUP NOT flag-Z! a! flag-Nc flag-Hs flag-Cc ; A3 4 update-arrays
: op-AND-H    a@ h@    AND DUP NOT flag-Z! a! flag-Nc flag-Hs flag-Cc ; A4 4 update-arrays
: op-AND-L    a@ l@    AND DUP NOT flag-Z! a! flag-Nc flag-Hs flag-Cc ; A5 4 update-arrays
: op-AND-(hl) a@ (hl)@ AND DUP NOT flag-Z! a! flag-Nc flag-Hs flag-Cc ; A6 8 update-arrays
: op-AND-n    a@ pc@+  AND DUP NOT flag-Z! a! flag-Nc flag-Hs flag-Cc ; E6 8 update-arrays

: op-OR-A    a@ a@    OR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; B7 4 update-arrays
: op-OR-B    a@ b@    OR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; B0 4 update-arrays
: op-OR-C    a@ c@    OR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; B1 4 update-arrays
: op-OR-D    a@ d@    OR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; B2 4 update-arrays
: op-OR-E    a@ e@    OR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; B3 4 update-arrays
: op-OR-H    a@ h@    OR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; B4 4 update-arrays
: op-OR-L    a@ l@    OR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; B5 4 update-arrays
: op-OR-(hl) a@ (hl)@ OR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; B6 8 update-arrays
: op-OR-n    a@ pc@+  OR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; F6 8 update-arrays

: op-XOR-A    a@ a@    XOR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; AF 4 update-arrays
: op-XOR-B    a@ b@    XOR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; A8 4 update-arrays
: op-XOR-C    a@ c@    XOR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; A9 4 update-arrays
: op-XOR-D    a@ d@    XOR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; AA 4 update-arrays
: op-XOR-E    a@ e@    XOR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; AB 4 update-arrays
: op-XOR-H    a@ h@    XOR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; AC 4 update-arrays
: op-XOR-L    a@ l@    XOR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; AD 4 update-arrays
: op-XOR-(hl) a@ (hl)@ XOR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; AE 8 update-arrays
: op-XOR-n    a@ pc@+  XOR DUP NOT flag-Z! a! flag-Nc flag-Hc flag-Cc ; EE 8 update-arrays

: op-CP-A     a@ a@    2DUP sub-helper-H sub-helper-C ( res ) NOT flag-Z! flag-Ns ; BF 4 update-arrays
: op-CP-B     a@ b@    2DUP sub-helper-H sub-helper-C ( res ) NOT flag-Z! flag-Ns ; B8 4 update-arrays
: op-CP-C     a@ c@    2DUP sub-helper-H sub-helper-C ( res ) NOT flag-Z! flag-Ns ; B9 4 update-arrays
: op-CP-D     a@ d@    2DUP sub-helper-H sub-helper-C ( res ) NOT flag-Z! flag-Ns ; BA 4 update-arrays
: op-CP-E     a@ e@    2DUP sub-helper-H sub-helper-C ( res ) NOT flag-Z! flag-Ns ; BB 4 update-arrays
: op-CP-H     a@ h@    2DUP sub-helper-H sub-helper-C ( res ) NOT flag-Z! flag-Ns ; BC 4 update-arrays
: op-CP-L     a@ l@    2DUP sub-helper-H sub-helper-C ( res ) NOT flag-Z! flag-Ns ; BD 4 update-arrays
: op-CP-(hl)  a@ (hl)@ 2DUP sub-helper-H sub-helper-C ( res ) NOT flag-Z! flag-Ns ; BE 8 update-arrays
: op-CP-n     a@ pc@+  2DUP sub-helper-H sub-helper-C ( res ) NOT flag-Z! flag-Ns ; FE 8 update-arrays

: impl-INC ( val -- val' )
    DUP F AND 1+ F > flag-H! ( val )
    1+
    DUP NOT flag-Z!
    flag-Nc
  ;

: op-INC-A    a@    impl-INC a!    ; 3C 4 update-arrays
: op-INC-B    b@    impl-INC b!    ; 04 4 update-arrays
: op-INC-C    c@    impl-INC c!    ; 0C 4 update-arrays
: op-INC-D    d@    impl-INC d!    ; 14 4 update-arrays
: op-INC-E    e@    impl-INC e!    ; 1C 4 update-arrays
: op-INC-H    h@    impl-INC h!    ; 24 4 update-arrays
: op-INC-L    l@    impl-INC l!    ; 2C 4 update-arrays
: op-INC-(hl) (hl)@ impl-INC (hl)! ; 34 C update-arrays


: impl-DEC ( val -- val' )
    DUP F AND 10 OR   1- F > flag-H!   ( val )
    1-
    DUP NOT flag-Z!
    flag-Ns
  ;

: op-DEC-A    a@    impl-DEC a!    ; 3D 4 update-arrays
: op-DEC-B    b@    impl-DEC b!    ; 05 4 update-arrays
: op-DEC-C    c@    impl-DEC c!    ; 0D 4 update-arrays
: op-DEC-D    d@    impl-DEC d!    ; 15 4 update-arrays
: op-DEC-E    e@    impl-DEC e!    ; 1D 4 update-arrays
: op-DEC-H    h@    impl-DEC h!    ; 25 4 update-arrays
: op-DEC-L    l@    impl-DEC l!    ; 2D 4 update-arrays
: op-DEC-(hl) (hl)@ impl-DEC (hl)! ; 35 C update-arrays



: impl-ADD-HL,n ( rhs -- )
    hl@ ( rhs lhs )
    2DUP FFF AND SWAP FFF AND ( rhs lhs lhs' rhs' )
    +      FFF > flag-H! ( rhs lhs )
    + DUP FFFF > flag-C! ( sum )
    hl!
    flag-Nc
  ;

: op-ADD-HL,BC bc@ impl-ADD-HL,n ; 09 8 update-arrays
: op-ADD-HL,DE de@ impl-ADD-HL,n ; 19 8 update-arrays
: op-ADD-HL,HL hl@ impl-ADD-HL,n ; 29 8 update-arrays
: op-ADD-HL,SP sp@ impl-ADD-HL,n ; 39 8 update-arrays

\ XXX This is messy and therefore suspicious.
: op-ADD-SP,n
    sp@ pc@+
    2DUP 80 >= IF \ negative
        100 SWAP - \ now it's a positive number to be subtracted
        2DUP FFF AND SWAP FFF AND 1000 OR SWAP -
        FFF > flag-H!
        SWAP 10000 OR SWAP
        -
        DUP FFFF > flag-C!
    ELSE \ positive
        2DUP FFF AND SWAP FFF AND +
        FFF > flag-H!
        +
        DUP FFFF > flag-C!
    THEN
    flag-Zc flag-Nc
    sp!
  ;
E8 16 update-arrays

: op-INC-BC bc@ 1+ bc! ; 03 8 update-arrays
: op-INC-DE de@ 1+ de! ; 13 8 update-arrays
: op-INC-HL hl@ 1+ hl! ; 23 8 update-arrays
: op-INC-SP sp@ 1+ sp! ; 33 8 update-arrays

: op-DEC-BC bc@ 1- bc! ; 0B 8 update-arrays
: op-DEC-DE de@ 1- de! ; 1B 8 update-arrays
: op-DEC-HL hl@ 1- hl! ; 2B 8 update-arrays
: op-DEC-SP sp@ 1- sp! ; 3B 8 update-arrays

\ Build the DAA table. It's a block as follows:
\ C before | Min upper | Max upper | H before | Min lower | Max lower | delta | C after
\ Each field is 1 byte wide, making the table 8 bytes wide and 12 rows tall, for
\ 96 bytes total. That's 0x68.
HERE @
60 ALLOT
0 C, 0 C, 9 C,   0 C, 0 C, 9 C,   00 C, 0 C,
0 C, 0 C, 8 C,   0 C, A C, F C,   06 C, 0 C,
0 C, 0 C, 9 C,   1 C, 0 C, 3 C,   06 C, 0 C,
0 C, A C, F C,   0 C, 0 C, 9 C,   60 C, 1 C,
0 C, 9 C, F C,   0 C, A C, F C,   66 C, 1 C,
0 C, A C, F C,   1 C, 0 C, 3 C,   66 C, 1 C,
1 C, 0 C, 2 C,   0 C, 0 C, 9 C,   60 C, 1 C,
1 C, 0 C, 2 C,   0 C, A C, F C,   66 C, 1 C,
1 C, 0 C, 3 C,   1 C, 0 C, 3 C,   66 C, 1 C,
0 C, 0 C, 8 C,   1 C, 6 C, F C,   FA C, 0 C,
1 C, 7 C, F C,   0 C, 0 C, 9 C,   A0 C, 1 C,
1 C, 6 C, F C,   1 C, 6 C, F C,   9A C, 1 C,

\ The address of the table is still on the stack.
CONSTANT daa-table

: check-daa-row ( row-addr -- ? )
       DUP C@ flag-C@ AND 0=      IF DROP FALSE EXIT THEN
    1+ DUP C@ a@ 4 RSHIFT F AND < IF DROP FALSE EXIT THEN
    1+ DUP C@ a@ r RSHIFT F AND > IF DROP FALSE EXIT THEN
    1+ DUP C@ flag-H@ AND 0=      IF DROP FALSE EXIT THEN
    1+ DUP C@ a@          F AND < IF DROP FALSE EXIT THEN
    1+ DUP C@ a@          F AND > IF DROP FALSE EXIT THEN
    DROP TRUE
  ;

: op-DAA
    12 0 DO
        I 8 * daa-table +
        DUP check-daa-row
        IF
            6 + DUP C@ 1+ C@ ( delta new-C )
            flag-C! a@ + a!  ( )
            1000
        ELSE
            DROP 1
        THEN
    +LOOP
  ;
27 4 update-arrays


: op-CPL a@ INVERT FF AND a! ; 2F 4 update-arrays

: op-CCF flag-C@ NOT flag-C! flag-Hc flag-Nc ; 3F 4 update-arrays
: op-SCF flag-Cs flag-Hc flag-Nc ; 37 4 update-arrays


: op-NOP ; 00 4 update-arrays

: op-HALT TRUE cpu-halted ! ; 76 4 update-arrays

\ Apparently this reads an extra byte? Doesn't really matter if it's always 0.
\ That's a NOP.
: op-STOP TRUE cpu-halted ! TRUE gpu-halted ! pc@+ ; 10 4 update-arrays

: op-DI -2 interrupts-future ! ; F3 4 update-arrays
: op-EI 2  interrupts-future ! ; FB 4 update-arrays


\ Generic rotate-left operator
\ Accepts the incoming new-bit-0 value and the main value,
\ returns the new value and the overflow bit.
\ Always sets C, since both flavours do that.
: impl-RL ( bit-0 val -- val' overflow )
    1 LSHIFT OR   ( val-oversized )
    DUP FF AND    ( val-oversized val' )
    SWAP 8 RSHIFT ( val' overflow )
    DUP flag-C!
    OVER 0= flag-Z!
    flag-Nc flag-Hc
  ;

: op-RLCA 0               a@ impl-RL OR   a! ; 07 4 update-arrays
: op-RLA  flag-C@ 1 AND   a@ impl-RL DROP a! ; 17 4 update-arrays

\ Generic rotate-right operator, see above for details.
: impl-RR ( bit-7 val -- val' overflow )
    DUP 1 AND       ( bit-7 val overflow )
    -ROT 1 RSHIFT   ( overflow bit-7 val-underflow )
    SWAP 0<> 80 AND ( overflow val-underflow bit-7 )
    OR SWAP         ( val' overflow )
    DUP flag-C!
    OVER 0= flag-Z!
    flag-Nc flag-Hc
  ;

: op-RRCA 0               a@ impl-RR 0<> 80 AND OR a! ; 0F 4 update-arrays
: op-RRA  flag-C@ 80 AND  a@ impl-RR DROP          a! ; 1F 4 update-arrays



\ Jump and return ops
: impl-JP ( ? -- ) pc@H+ SWAP IF pc! ELSE DROP THEN ;
: op-JP    TRUE        impl-JP ; C3 C update-arrays
: op-JP-NZ flag-Z@ NOT impl-JP ; C2 C update-arrays
: op-JP-Z  flag-Z@     impl-JP ; CA C update-arrays
: op-JP-NC flag-C@ NOT impl-JP ; D2 C update-arrays
: op-JP-C  flag-C@     impl-JP ; DA C update-arrays

: op-JP-HL hl@ pc! ; E9 4 update-arrays

: impl-JR ( ? -- ) IF pc@+ DUP 80 AND   IF 100 SWAP - THEN pc+ ELSE pc++ THEN ;

: op-JR    TRUE        impl-JR ; 18 8 update-arrays
: op-JR-NZ flag-Z@ NOT impl-JR ; 20 8 update-arrays
: op-JR-Z  flag-Z@     impl-JR ; 28 8 update-arrays
: op-JR-NC flag-C@ NOT impl-JR ; 30 8 update-arrays
: op-JR-C  flag-C@     impl-JR ; 38 8 update-arrays


: impl-CALL ( ? -- )
    IF   reg-pc H@ pushH   pc@H+ pc!
    ELSE 2 pc+ THEN
  ;

: op-CALL    TRUE        impl-CALL ; CD C update-arrays
: op-CALL-NZ flag-Z@ NOT impl-CALL ; C4 C update-arrays
: op-CALL-Z  flag-Z@     impl-CALL ; CC C update-arrays
: op-CALL-NC flag-C@ NOT impl-CALL ; D4 C update-arrays
: op-CALL-C  flag-C@     impl-CALL ; DC C update-arrays


: impl-RST reg-pc H@ pushH   pc! ;
: op-RST-00 00 impl-RST ; C7 20 update-arrays
: op-RST-08 08 impl-RST ; CF 20 update-arrays
: op-RST-10 10 impl-RST ; D7 20 update-arrays
: op-RST-18 18 impl-RST ; DF 20 update-arrays
: op-RST-20 20 impl-RST ; E7 20 update-arrays
: op-RST-28 28 impl-RST ; EF 20 update-arrays
: op-RST-30 30 impl-RST ; F7 20 update-arrays
: op-RST-38 38 impl-RST ; FF 20 update-arrays


: impl-RET ( ? -- ) IF popH pc! THEN ;
: op-RET    TRUE        impl-RET ; C9 8 update-arrays
: op-RET-NZ flag-Z@ NOT impl-RET ; C0 8 update-arrays
: op-RET-Z  flag-Z@     impl-RET ; C8 8 update-arrays
: op-RET-NC flag-C@ NOT impl-RET ; D0 8 update-arrays
: op-RET-C  flag-C@     impl-RET ; D8 8 update-arrays

: op-RETI popH pc!  TRUE interrupt-enable ! ; D9 8 update-arrays


100 ARRAY cb-opcodes
100 ARRAY cb-cycles

: op-extension pc@+   DUP cb-cycles @ delay-cycles +!   cb-opcodes @ EXECUTE ;
CB 0 update-arrays


: update-cb-arrays ( opcode cycles -- )
    OVER             ( opcode cycles opcode )
    LATEST @ (>CODE) ( opcode cycles opcode xt )
    SWAP cb-opcodes !   ( opcode cycles )
    SWAP cb-cycles  !   ( )
  ;


: impl-SWAP ( val -- val' )
    DUP  F  AND 4 LSHIFT ( val new-hi )
    SWAP F0 AND 4 RSHIFT ( new-hi new-lo )
    OR
    DUP NOT flag-Z!
    flag-Nc flag-Hc flag-Cc
  ;
: op-SWAP-A    a@    impl-SWAP a!    ; 37 8  update-cb-arrays
: op-SWAP-B    b@    impl-SWAP b!    ; 30 8  update-cb-arrays
: op-SWAP-C    c@    impl-SWAP c!    ; 31 8  update-cb-arrays
: op-SWAP-D    d@    impl-SWAP d!    ; 32 8  update-cb-arrays
: op-SWAP-E    e@    impl-SWAP e!    ; 33 8  update-cb-arrays
: op-SWAP-H    h@    impl-SWAP h!    ; 34 8  update-cb-arrays
: op-SWAP-L    l@    impl-SWAP l!    ; 35 8  update-cb-arrays
: op-SWAP-(hl) (hl)@ impl-SWAP (hl)! ; 36 10 update-cb-arrays


: op-RLC-A    0    a@ impl-RL OR    a! ; 07 8  update-cb-arrays
: op-RLC-B    0    b@ impl-RL OR    b! ; 00 8  update-cb-arrays
: op-RLC-C    0    c@ impl-RL OR    c! ; 01 8  update-cb-arrays
: op-RLC-D    0    d@ impl-RL OR    d! ; 02 8  update-cb-arrays
: op-RLC-E    0    e@ impl-RL OR    e! ; 03 8  update-cb-arrays
: op-RLC-H    0    h@ impl-RL OR    h! ; 04 8  update-cb-arrays
: op-RLC-L    0    l@ impl-RL OR    l! ; 05 8  update-cb-arrays
: op-RLC-(hl) 0 (hl)@ impl-RL OR (hl)! ; 06 10 update-cb-arrays

: op-RL-A    flag-C@ 1 AND    a@ impl-RL DROP    a! ; 17 8  update-cb-arrays
: op-RL-B    flag-C@ 1 AND    b@ impl-RL DROP    b! ; 10 8  update-cb-arrays
: op-RL-C    flag-C@ 1 AND    c@ impl-RL DROP    c! ; 11 8  update-cb-arrays
: op-RL-D    flag-C@ 1 AND    d@ impl-RL DROP    d! ; 12 8  update-cb-arrays
: op-RL-E    flag-C@ 1 AND    e@ impl-RL DROP    e! ; 13 8  update-cb-arrays
: op-RL-H    flag-C@ 1 AND    h@ impl-RL DROP    h! ; 14 8  update-cb-arrays
: op-RL-L    flag-C@ 1 AND    l@ impl-RL DROP    l! ; 15 8  update-cb-arrays
: op-RL-(hl) flag-C@ 1 AND (hl)@ impl-RL DROP (hl)! ; 16 10 update-cb-arrays

: op-RRC-A    0    a@ impl-RR 0<> 80 AND OR    a! ; 0F 8  update-cb-arrays
: op-RRC-B    0    b@ impl-RR 0<> 80 AND OR    b! ; 08 8  update-cb-arrays
: op-RRC-C    0    c@ impl-RR 0<> 80 AND OR    c! ; 09 8  update-cb-arrays
: op-RRC-D    0    d@ impl-RR 0<> 80 AND OR    d! ; 0A 8  update-cb-arrays
: op-RRC-E    0    e@ impl-RR 0<> 80 AND OR    e! ; 0B 8  update-cb-arrays
: op-RRC-H    0    h@ impl-RR 0<> 80 AND OR    h! ; 0C 8  update-cb-arrays
: op-RRC-L    0    l@ impl-RR 0<> 80 AND OR    l! ; 0D 8  update-cb-arrays
: op-RRC-(hl) 0 (hl)@ impl-RR 0<> 80 AND OR (hl)! ; 0E 10 update-cb-arrays

: op-RR-A    flag-C@ 80 AND     a@ impl-RR DROP    a! ; 1F 8  update-cb-arrays
: op-RR-B    flag-C@ 80 AND     b@ impl-RR DROP    b! ; 18 8  update-cb-arrays
: op-RR-C    flag-C@ 80 AND     c@ impl-RR DROP    c! ; 19 8  update-cb-arrays
: op-RR-D    flag-C@ 80 AND     d@ impl-RR DROP    d! ; 1A 8  update-cb-arrays
: op-RR-E    flag-C@ 80 AND     e@ impl-RR DROP    e! ; 1B 8  update-cb-arrays
: op-RR-H    flag-C@ 80 AND     h@ impl-RR DROP    h! ; 1C 8  update-cb-arrays
: op-RR-L    flag-C@ 80 AND     l@ impl-RR DROP    l! ; 1D 8  update-cb-arrays
: op-RR-(hl) flag-C@ 80 AND  (hl)@ impl-RR DROP (hl)! ; 1E 10 update-cb-arrays


\ Shifts
: impl-SLA ( val -- val' )
    1 LSHIFT
    DUP 100 AND flag-C!
    FF AND
    DUP 0= flag-Z!
    flag-Nc flag-Hc
  ;

: op-SLA-A       a@ impl-SLA    a! ; 27 8  update-cb-arrays
: op-SLA-B       b@ impl-SLA    b! ; 20 8  update-cb-arrays
: op-SLA-C       c@ impl-SLA    c! ; 21 8  update-cb-arrays
: op-SLA-D       d@ impl-SLA    d! ; 22 8  update-cb-arrays
: op-SLA-E       e@ impl-SLA    e! ; 23 8  update-cb-arrays
: op-SLA-H       h@ impl-SLA    h! ; 24 8  update-cb-arrays
: op-SLA-L       l@ impl-SLA    l! ; 25 8  update-cb-arrays
: op-SLA-(hl) (hl)@ impl-SLA (hl)! ; 26 10 update-cb-arrays


: impl-SRA ( val -- val' )
    DUP 1  AND flag-C! ( val )
    DUP 80 AND         ( val MSB )
    SWAP 1 RSHIFT OR   ( val' )
    DUP 0= flag-Z!
    flag-Nc flag-Hc
  ;

: op-SRA-A       a@ impl-SRA    a! ; 2F 8  update-cb-arrays
: op-SRA-B       b@ impl-SRA    b! ; 28 8  update-cb-arrays
: op-SRA-C       c@ impl-SRA    c! ; 29 8  update-cb-arrays
: op-SRA-D       d@ impl-SRA    d! ; 2A 8  update-cb-arrays
: op-SRA-E       e@ impl-SRA    e! ; 2B 8  update-cb-arrays
: op-SRA-H       h@ impl-SRA    h! ; 2C 8  update-cb-arrays
: op-SRA-L       l@ impl-SRA    l! ; 2D 8  update-cb-arrays
: op-SRA-(hl) (hl)@ impl-SRA (hl)! ; 2E 10 update-cb-arrays


: impl-SRL ( val -- val' )
    DUP 1  AND flag-C! ( val )
    1 RSHIFT           ( val' )
    DUP 0= flag-Z!
    flag-Nc flag-Hc
  ;

: op-SRL-A       a@ impl-SRL    a! ; 3F 8  update-cb-arrays
: op-SRL-B       b@ impl-SRL    b! ; 38 8  update-cb-arrays
: op-SRL-C       c@ impl-SRL    c! ; 39 8  update-cb-arrays
: op-SRL-D       d@ impl-SRL    d! ; 3A 8  update-cb-arrays
: op-SRL-E       e@ impl-SRL    e! ; 3B 8  update-cb-arrays
: op-SRL-H       h@ impl-SRL    h! ; 3C 8  update-cb-arrays
: op-SRL-L       l@ impl-SRL    l! ; 3D 8  update-cb-arrays
: op-SRL-(hl) (hl)@ impl-SRL (hl)! ; 3E 10 update-cb-arrays



: make-mask ( bit -- mask ) 1 SWAP LSHIFT ;

: impl-BIT ( val bit -- ) make-mask AND  flag-Nc flag-Hs 0= flag-Z! ;

: op-BIT-A       a@ pc@+ impl-BIT ; 47  8 update-cb-arrays
: op-BIT-B       b@ pc@+ impl-BIT ; 40  8 update-cb-arrays
: op-BIT-C       c@ pc@+ impl-BIT ; 41  8 update-cb-arrays
: op-BIT-D       d@ pc@+ impl-BIT ; 42  8 update-cb-arrays
: op-BIT-E       e@ pc@+ impl-BIT ; 43  8 update-cb-arrays
: op-BIT-H       h@ pc@+ impl-BIT ; 44  8 update-cb-arrays
: op-BIT-L       l@ pc@+ impl-BIT ; 45  8 update-cb-arrays
: op-BIT-(hl) (hl)@ pc@+ impl-BIT ; 46 10 update-cb-arrays

: op-SET-A       a@ pc@+ make-mask OR    a! ; C7  8 update-cb-arrays
: op-SET-B       b@ pc@+ make-mask OR    b! ; C0  8 update-cb-arrays
: op-SET-C       c@ pc@+ make-mask OR    c! ; C1  8 update-cb-arrays
: op-SET-D       d@ pc@+ make-mask OR    d! ; C2  8 update-cb-arrays
: op-SET-E       e@ pc@+ make-mask OR    e! ; C3  8 update-cb-arrays
: op-SET-H       h@ pc@+ make-mask OR    h! ; C4  8 update-cb-arrays
: op-SET-L       l@ pc@+ make-mask OR    l! ; C5  8 update-cb-arrays
: op-SET-(hl) (hl)@ pc@+ make-mask OR (hl)! ; C6 10 update-cb-arrays

: op-RES-A       a@ pc@+ make-mask INVERT AND    a! ; 87  8 update-cb-arrays
: op-RES-B       b@ pc@+ make-mask INVERT AND    b! ; 80  8 update-cb-arrays
: op-RES-C       c@ pc@+ make-mask INVERT AND    c! ; 81  8 update-cb-arrays
: op-RES-D       d@ pc@+ make-mask INVERT AND    d! ; 82  8 update-cb-arrays
: op-RES-E       e@ pc@+ make-mask INVERT AND    e! ; 83  8 update-cb-arrays
: op-RES-H       h@ pc@+ make-mask INVERT AND    h! ; 84  8 update-cb-arrays
: op-RES-L       l@ pc@+ make-mask INVERT AND    l! ; 85  8 update-cb-arrays
: op-RES-(hl) (hl)@ pc@+ make-mask INVERT AND (hl)! ; 86 10 update-cb-arrays

DECIMAL


\ Interpreter proper
VARIABLE video-counter   0 video-counter !
VARIABLE timer-base      0 timer-base !

\ timer-base counts instructions.
\ Every 64 instructions, it bumps the timer divider, DIV.
\ TAC chooses the frequency for TIMA.
\   00:   4096 Hz 256 instructions
\   01: 262144 Hz   4 instructions
\   10:  65536 Hz  16 instructions
\   11:  16384 Hz  64 instructions
4 ARRAY timer-frequency-masks
255 0 timer-frequency-masks !
  3 1 timer-frequency-masks !
 15 2 timer-frequency-masks !
 63 3 timer-frequency-masks !

: update-timers ( )
    timer-base @ 1+ DUP timer-base !
    DUP 63 AND 0= IF io-DIV io-ports DUP C@ 1+ SWAP C! THEN
    ( base )
    \ Read the timer frequency from TAC.
    io-TAC io-ports C@ ( base tac )
    DUP 4 AND IF ( base tac )
        3 AND timer-frequency-masks @ ( base mask )
        AND 0= IF
            \ Timer bump. Read it in and bump it.
            io-TMA io-ports DUP C@ ( c-addr timer )
            1+ 255 AND             ( c-addr timer' )
            2DUP SWAP C!           ( c-addr timer' )
            NIP                    ( timer' )
            0= IF
                \ Timer overflow, set the timer interrupt IF bit.
                \ Also rewrite TIMA to TMA.
                io-TMA io-ports C@ io-TIMA io-ports C!
                io-IF io-ports DUP C@ int-mask-timer OR SWAP C!
            THEN
        THEN
    ELSE 2DROP
    THEN
  ;

: mode@ ( -- STAT-mode ) io-STAT io-ports C@ 3 AND ;
: mode! ( mode -- )
    3 AND   io-STAT io-ports ( mode addr )
    DUP C@ 3 INVERT AND ROT OR ( addr mode' )
    SWAP C!
  ;

: enable-interrupt ( mask -- )
    io-IF io-ports DUP C@ ( mask addr value )
    ROT OR SWAP C!
  ;


: maybe-fire-stat-int ( mask -- )
    io-STAT io-ports C@ AND IF
        int-mask-stat enable-interrupt
    THEN
  ;



\ Video mode handling.
\ The process: video-counter ticks from 0 through 113, and repeats.
\ On 0,   set mode 2 (and reset counter to 0)
\ On 20,  set mode 3 and render the current LY.
\ On 63,  set mode 0.
\ On 114, bump LY. If it's now 144, set VBlank.
\ When VBlank is set, LY will be >= 144. Then just cycle to 114 and keep
\ incrementing LY. When LY reaches 154, reset to 0 and unlock VBlank.
: video-start-vblank ( -- )
    int-mask-vblank enable-interrupt
    stat-int-vblank maybe-enable-stat-interrupt
    1 mode!
  ;

: video-bump-ly ( counter ly -- counter' ly' )
    OVER 114 = IF
        0 video-counter ! ( old-counter ly )
        SWAP DROP 0 SWAP  ( new-counter ly )
        1+ DUP io-LY io-ports C! ( new-counter ly' )
    THEN
  ;

: video-check-vblank ( ly -- vblank? )
    DUP 144 >= IF ( ly ) \ VBlank period
        TRUE SWAP CASE ( vblank? )
        144 OF video-start-vblank ENDOF
        154 OF 0 io-LY io-ports C!   DROP FALSE ENDOF \ LY=0, end VBlank
        \ Mode gets reset (to 2) by later phases of update-video.
        ENDCASE ( vblank? )
    ELSE FALSE THEN
  ;


\ I defined counter 0 to be the start of the Mode 2 phase.
: update-video ( -- )
    \ Several interconnected rules here.
    \ First, if the display is not enabled, just bail.
    io-LCDC io-ports C@
    lcdc-enable AND NOT IF EXIT THEN ( )
    gpu-halted @ IF EXIT THEN ( )

    \ LCD enabled, so compare the mode and the counter.
    video-counter @    ( counter )
    io-LY io-ports C@  ( counter ly )
    video-bump-ly      ( counter ly )
    video-check-vblank ( counter vblank? )
    \ If we're in VBlank, then there's nothing to do.
    \ If not, then run the usual cycles.
    NOT IF ( counter )
        CASE ( )
          0 OF 2 mode!   stat-int-oam maybe-fire-stat-int ENDOF
         20 OF 3 mode!   gpu-draw-line ENDOF
         63 OF 0 mode!   stat-int-hblank maybe-fire-stat-int ENDOF
        ENDCASE ( )
    THEN ( )
  ;


\ If interrupt-future is nonzero, it moves one step towards 0.
\ When it moves to 0, interrupts become enabled or disabled.
\ NB: This is only called when nonzero!
: update-interrupt-future ( -- )
    interrupt-future @ CASE
    -2 OF -1 ENDOF
     2 OF  1 ENDOF
    -1 OF  0 FALSE interrupt-enable ! ENDOF
     1 OF  0 TRUE  interrupt-enable ! ENDOF
    ENDCASE
    interrupt-future !
  ;


\ Checks for interrupts. They need to be enabled (EI) and an unmasked interrupt
\ must be triggered.
\ Interrupts are served in priority order (lowest bit number).
\ When an interrupt fires, interrupts are disabled, the flag is cleared from IF,
\ PC is pushed, and PC is moved to the interrupt vector.
: check-interrupts ( -- )
    interrupt-enable @
    io-IF io-ports C@
    io-IE io-ports C@ ( ? ask-mask enable-mask )
    AND AND DUP IF ( triggered-mask )
        \ Now run from highest to lowest priority. We compute the vector
        \ directly from the counter.
        5 0 DO ( triggered-mask )
            1 I LSHIFT ( triggered-mask check-mask )
            OVER AND ( triggered ? )
            IF DROP I 1000 ELSE 1 THEN
        +LOOP ( index )
        1 OVER LSHIFT INVERT  ( index disabling-mask )
        io-IF io-ports DUP C@ ( index disabling-mask addr if )
        ROT AND SWAP C!       ( index )

        reg-pc H@ pushH ( index )
        8 * 64 + ( new-pc )
        pc!      ( )
        FALSE interrupt-enable !
    ELSE DROP THEN
    ( )
  ;


\ Actually executes a single instruction.
: run-instruction ( )
    pc@+   DUP cycles @ delay-cycles !   opcodes @ EXECUTE
  ;

\ Main interpreter code
\ Steps:
\ - Tick timers and video counters.
\ - Wait out instruction delay.
\ - If not done cycling, continue the loop.
\ - Check for interrupts, which updates PC if interrupting.
\ - Run an instruction.
\ - Repeat forever.
: interp
    BEGIN
        1 timer-base +!
        update-timers
        update-video

        \ Only run anything if the delay counter is 0
        delay-cycles @ 4 - DUP delay-cycles ! ( delay-cycles )
        0= IF
            \ Special-case this since 0 is by far the most likely.
            interrupt-future @ 0<> IF update-interrupt-future THEN

            check-interrupts \ Will set the PC, if there are interrupts.
            cpu-halted @ NOT IF run-instruction THEN
        THEN
    AGAIN
  ;


