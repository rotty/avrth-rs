: case
    0
; immediate

: of
    1+ >r  \ count `of's, put that on off stack
    ['] (of) i,
    ['] if i,
    r> \ get #of onto the stack again
; immediate

: endof
    >r ['] else i, r>
; immediate

: endcase
    ['] drop i, ?do
        ['] then i,
    loop
; immediate

: (does>)
    dp ( -- addr )
    ['] (:) i, \ start colon definition
    get-current i, \ compile invocation of currently created word
    set-current \ ( -- ) switch that word's xt to this place
    \ the following compiles to a Forth VM jump to our return address
    ['] (literal) i, r> i,
    ['] ((does>)) i,
;

: does>
    ['] (does>) i,
; immediate

: literal
    ['] (literal) i, i,
; immediate

: sp0 [parameter] sp0 literal ;

: rp0 [parameter] rp0 literal ;

: depth ( -- n )
    sp0 sp@ - cells / 1-
;
