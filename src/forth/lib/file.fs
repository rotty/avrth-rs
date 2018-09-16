: r/o ( -- fam )
    1
;

: r/w ( -- fam )
    3
;

: w/o ( -- fam )
    2
;

: bin ( fam1 -- fam2 )
;

: push-source ( fileid -- )
    source >in @ ,push-source
    0 #tib ! 0 >in !
;

: pop-source ( -- )
    tib tibsize ,pop-source >in !
;

: execute-parsing-file ( fileid xt -- )
    swap push-source execute pop-source
;

: read-loop
    begin
        refill
    while
            interpret
    repeat
;

: include-file ( fileid -- )
    ['] read-loop execute-parsing-file
;

: include
    parse-name open-file ?dup if
        throw
    else
        include-file
    then
;

: string-table-add ( c-addr u tableid -- )
    >r 2>r -1 2r> r> string-table-insert
;

: included-files
    [ create-string-table ] literal
;

: included? ( addr u -- flag )
    included-files string-table-contains?
;

\ Add a filename to the list of included files
: add-included-file ( addr u -- )
    included-files string-table-add
;

: ofile
    [ here ] literal [ 255 allot ]
;

: open-path-file ( addr1 u1 path-addr - wfileid addr2 u2 0 | ior )
    ofile 255 ,open-path-file
;

: fpath ( -- pathid )
    [parameter] fpath literal
;

: require ( "name" -- )
    parse-name fpath open-path-file throw
    2dup included? if
        2drop close-file
    else
        add-included-file include-file
    then
;
