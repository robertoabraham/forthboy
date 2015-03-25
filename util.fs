\ Forth GB (c) 2015 Braden Shepherdson
\ Version 1
\ Miscellaneous utilities


: CARRAY ( u "name" -- ) ( exec: index -- c-addr )
    CREATE ALLOT DOES> + ;

: KB ( u -- u ) 10 LSHIFT ;

