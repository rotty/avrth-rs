: handler
    [ here ] literal [ 0 , ]
;

: throw ( n -- )
    dup 0= if
        drop exit
    then
    handler @ rp! r> handler ! r> swap >r sp! drop r>
;

: catch ( xt -- )
    sp@ >r handler @ >r rp@ handler !
    execute r> handler ! r> drop 0
;
