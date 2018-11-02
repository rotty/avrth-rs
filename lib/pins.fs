require 2c.fs
require ports.fs

\
\ TODO: Taken from amforth/lib/bitnames.frt, probably integrate that
\ file wholesale
\

: pinport>u
    1 swap lshift swap 2c>u
;

\ At compiletime:
\ Store combination of portaddress and bit number in a cell and give it a name.
\ At runtime:
\ Get pinmask and portaddress on stack.
: pin: icreate ( C: "ccc" portadr n -- ) ( R: -- pinmask portadr )
    pinport>u i, \ packed value
  does> i@                      \ get packed value
    u>2c
;

: pins: icreate ( C: "ccc" portadr pinmask -- ) ( R: -- pinmask portadr )
    swap 2c>u i,
  does> i@
    u>2c
;

\ pin>pos
\     convert bitmask of portpin: back to value (bitposition)
: pin>pos       ( pinmask portaddr -- pos )
  drop          ( -- pinmask )
  log2          ( -- pos_of_most_significant_bit )
;

\ Turn a port pin on, dont change the others.
: pin-high ( pinmask portadr -- )
    port-or!
;

\ Turn a port pin off, dont change the others.
: pin-low ( pinmask portadr -- )
    swap invert swap port-and!
;

\ Only for PORTx bits, 
\ because address of DDRx is one less than address of PORTx.

\ Set DDRx so its corresponding pin is output.
: pin-output ( pinmask portadr -- )
    1- pin-high
;

\ Set DDRx so its corresponding pin is input.
: pin-input  ( pinmask portadr -- )   
    1- pin-low
;
