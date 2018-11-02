\ pack two 8-bit values into a 16-bit cell
: 2c>u ( u1 u2 -- u )
    8 lshift or
;


: u>2c ( u -- u1 u2 )
    dup $FF and swap 8 rshift
;
