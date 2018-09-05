\
\ Arithmetic
\
: negate
    invert 1+
;

: within ( n min max -- f )
    >r over > swap r> > or 0=
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
\ Misc
\
: ms
    0 ?do 1ms loop
;

\
\ Control flow
\
: (of)
    over = if drop 1 else 0 then
;
