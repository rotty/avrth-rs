\
\ Arithmetic
\
: negate
    invert 1+
;

: within ( n min max -- f )
    >r over > swap r> > or 0=
;

: min ( n1 n2 -- n )
    over over > if swap then drop
;

: d>s
    drop
;

: *
    m* d>s
;

: /
    /mod nip
;

: mod
    /mod drop
;

\
\ Memory
\
: cells 2* ; \ FIXME: assumes 16 bit cells
: cell 2 ; \ likewise

\
\ Stack manipulation
\
: 2swap
    rot >r rot r>
;

: 2dup
    over over
;

: 2drop
    drop drop
;

: tuck
    swap over
;

\
\ Misc
\
: ms
    0 ?do 1ms loop
;

\
\ Strings
\
: (sliteral)
    r> dup dup i@ swap 1+ swap rot over 1+ 2/ + 1+ >r
;

\
\ Control flow
\
: (of)
    over = if drop 1 else 0 then
;
