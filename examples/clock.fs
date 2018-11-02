require simple/clock.fs

: run-clock ( -- )
    +clock
    +int
    begin
        10 ms
    again
;
