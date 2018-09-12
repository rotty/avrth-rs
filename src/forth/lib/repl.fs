\
\ String-related words
\
: digit? ( c base -- number flag )
    >r dup 57 > if
        223 and 65 10 >r
    else
        48 0 >r
    then
    - r> + dup 0 r> within
;

: count ( c-addr1 -- c-addr2 u )
    dup 1+ swap c@
;

: /string ( addr1 u1 n -- addr2 u2 )
    over min rot over + rot rot -
;

: cscan ( addr1 n1 c -- addr1 n2 )
    swap dup >r 0 ?do \ addr1 c
        over i + c@ over = if
            drop i unloop rdrop exit
        then
    loop
    drop r>
;

: cskip ( addr1 n1 c -- addr2 n2 )
    >r
    begin
        dup
    while
            over c@ r@ = if
                1 /string
            else
                rdrop exit
            then
    repeat
    rdrop
;

: place over over c! 1+ swap cmove> ;

: base
    [ here ] literal [ 10 , ]
;

: decimal 10 base ! ;

: hex 16 base ! ;

: bin 2 base ! ;

: setbase ( c -- )
    dup 36 = if drop hex exit then \ ASCII '$'
    dup 37 = if drop bin exit then \ ASCII '%'
    38 = if drop decimal exit then \ ASCII '&'
;

: praefix ( addr1 len1 -- addr2 len2 )
  over c@ 41 > if exit then
  over c@ setbase 1 /string
;

: >number ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
    begin
        dup
    while
            over c@ base @ digit? 0= if
                drop exit
            then
            >r 2swap r> swap base @ um* drop rot
            base @ um* d+ 2swap 1 /string
    repeat
;

: number ( addr -- n )
    base @ >r
    count over c@ 45 = dup >r if \ 45 is ASCII '-'
      1 /string
    then
    praefix >r >r 0 0 r> r> >number if
        -13 throw
    then
    drop drop r> if negate then r> base !
;

\
\ Compiler support
\
: state
    [ here ] literal [ 0 , ]
;

: find ( addr -- [ addr 0 ] | [ xt [-1|1]] )
    dup c@ swap 1+ swap find-name ?dup 0= if 1- 0 then
;

: \
    source nip >in !
; immediate

: parse ( char "ccc<char>" -- c-addr u )
    >r source >in @ /string r> cscan dup 1+ >in +!
;

: parse-word ( char -- c-addr u )
    >r source >in @ /string swap over r@
    cskip rot over - >in +! r> cscan dup 1+
    >in +!
;

: parse-name ( "name" -- c-addr u )
    bl parse-word
;

: '
    parse-name find-name 0= if
        drop -14 throw
    then
;

: [']
  ['] (literal) i, ' i,
; immediate

: word
    parse-word here place
    0 here dup c@ + 1+ c! \ zero-terminate
    here
;

: (create:)
    parse-name (create) ['] (:) i,
;

: create
    (create:) ['] (literal) i, here i, ['] exit i,
;

: icreate
    (create:) ['] (literal) i, dp 2 + i, ['] exit i,
;

: variable
    (create:) ['] (literal) i, here i,
    cell allot ['] exit i,
;

: constant
    (create:) ['] (literal) i, i, ['] exit i,
;

: 2constant
    (create:) swap
    ['] (literal) i, i,
    ['] (literal) i, i,
    ['] exit i,
;

: [
    0 state !
; immediate

: ]
    1 state !
;

: : ( -- )
    (create:) 1 state !
;

: ; ( -- )
    ['] exit i, 0 state !
; immediate

: (
    41 word drop
; immediate


\
\ The "REPL"
\
: interpret
    begin
        bl word dup c@ 0>
    while
            find ?dup 0= if
                number state @ if ['] (literal) i, i, then
            else
                0> if
                    execute
                else
                    state @ if i, else execute then
                then
            then
    repeat
    drop
;

: quit
  -1 begin \ f
      if sp0 sp! rp0 rp! 0 state ! then
      state @ 0= if
          cr s" > " itype
      then
      refill
  while
          ['] interpret catch ?dup if
              dup -2 < if
                  s" ?? " itype
                  base @ >r decimal . >in @ . r> base !
              then
              -1
          else
              s" ok" itype
              0
          then
  repeat
;
