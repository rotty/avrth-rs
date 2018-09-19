: bl 32 ;

: cr 10 emit ;
: space bl emit ;

: itype ( i-addr n -- )
    0 swap ( i-addr 0 n )
    0 ?do ( i-addr acc )
        [ cell 1- ] literal i and 0= if
            drop dup 1+ swap i@ ( i-addr' acc')
        then
        dup emit
        8 rshift
    loop
    2drop
;

: type ( c-addr n -- )
  0 ?do
    dup i + c@ emit
  loop drop
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
