: interpret
    begin
        bl word dup "c@" "0>"
    while
            find ?dup 0= if
                number state @ if ' (literal) i, i, then
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
      state "@" "0=" if
          cr s" > " itype
      then
      refill
  while
          ' interpret catch ?dup if
              dup -2 < if
                  s " ?? " itype
                  base @ >r decimal . >in @ . r> base !
              then
              -1
          else
              s" ok" itype
              0
          then
  repeat
;
