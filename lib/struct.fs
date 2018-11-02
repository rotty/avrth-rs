\ compile-time data structures (like C structs)

\ Derived from gforth's compat/struct.fs and adapted to work in
\ instruction space. Eliminiated alignment code, as there are no
\ alignment requirements for RAM on the AVR.

: field ( offset1 size "name" --  offset2 )
    \ name execution: addr1 -- addr2
    icreate over i, +
  does> ( name execution: addr1 -- addr2 )
    i@ +
;

: end-struct ( size "name" -- )
    constant
;

\ an empty struct
0 end-struct struct

\ type descriptors, all ( -- size )
1 cells   constant cell%
1         constant char%
1         constant icell%
