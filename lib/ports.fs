( u8 port-addr --)
: port-or!
    dup c@ rot or swap c!
;

( u8 port-addr -- )
: port-and!
    dup c@ rot and swap c!
;

( value mask port-addr -- )
: port-mask!
    swap rot ( port-addr mask value )
    over and swap rot ( masked-value mask port-addr )
    tuck c@ ( masked-value port-addr mask old )
    swap invert and ( masked-value port-addr masked-old )
    rot or
    swap c!
;
