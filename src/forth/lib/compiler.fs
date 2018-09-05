: <mark
  dp
;

: <resolve
    i,
;

: >mark
    dp 0 i,
;

: >resolve
  dp swap i!
;

: if
    ' (0branch) i, >mark
; immediate

: else
    ' (branch) i, >mark swap >resolve
; immediate

: then
    >resolve
; immediate

: begin
    <mark
; immediate

: again
    ' (branch) i, <resolve
; immediate

: until
    ' (0branch) i,  <resolve
; immediate

: while
    ' (0branch) i,  >mark swap
; immediate

: repeat
    ' (branch) i,  <resolve >resolve
; immediate

: ?do
    ' (?do) i, >mark <mark
; immediate

: loop
    ' (loop) i, <resolve >resolve
; immediate

: +loop
    ' (+loop) i, <resolve >resolve
; immediate
