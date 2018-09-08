: bl 32 ;

: cr 10 emit ;
: space bl emit ;

: itype ( c-addr n -- )
    dup 2/ swap over 2* - >r
    0 ?do
        dup i@ dup emit 8 rshift emit 1+
    loop
    r> 0> if
        dup i@ emit
    then drop
;

: tibsize 128 ;

: tib
    [ here ] literal [ tibsize allot ]
;

: #tib
    [ here ] literal [ 0 , ]
;

: source
    tib #tib @
;

: >in
    [ here ] literal [ 0 , ]
;

: eof
    [ here ] literal [ 0 , ]
;

: /key ;

: accept ( addr n1 -- n2 )
  0 eof !
    swap over 0 ?do ( n1 addr -- )
        key dup 10 = over 13 = or if \ '\n' or '\r'
            drop nip i swap leave
        then
        dup -1 = if \ EOF
            -1 eof ! drop nip i swap leave
        then
        dup 8 = if \ backspace
            i 0 <> if
                dup emit space emit \ erase input echo
            then
            0
        else
            dup bl < if drop bl then
            dup emit over c!
            1+ 1
        then
    +loop
        drop /key cr
;

: refill ( -- f )
    tib tibsize accept #tib ! 0 >in ! eof @ 0=
;
