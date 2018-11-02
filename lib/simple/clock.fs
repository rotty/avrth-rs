require units/timer0.fs

%101 %10 %01 %00 timer8-settings constant clock-settings

variable clock

( -- )
: clock-isr
    1 clock +!
;

: get-clock
    clock @
;

: +clock ( -- )
    [ 10 1024 device-option ms-cycles ] literal timer0 timer8-comp-a!
    clock-settings timer0 +timer8
    ['] clock-isr TIMER8-COMP-A timer0 timer8-int!
    TIMER8-COMP-A timer0 +timer8-int
;
