require struct.fs
require pins.fs
require ports.fs

\ ++missing: overflow ISR
struct
    icell%     field timer8-ocr-a&b
    icell%     field timer8-cr-a&b
    icell%     field timer8-imsk&ifr
    icell% 3 * field timer8-isrs
    icell%     field timer8-cnt
    icell%     field timer8-pin-a
    icell%     field timer8-pin-b
end-struct timer8%

: create-timer8 ( ocr-a ocr-b cr-a cr-b cnt imsk ifr isr-ovf isr-a isr-b pin-a pin-b "name" -- )
    icreate dp 1+ dup >r i, timer8% iallot
    pinport>u r@ timer8-pin-b i!
    pinport>u r@ timer8-pin-a i!
    r@ timer8-isrs
    -1 2 ?do
        tuck i + i!
    -1 +loop drop
    2c>u r@ timer8-imsk&ifr i!
    r@ timer8-cnt i!
    2c>u r@ timer8-cr-a&b i!
    2c>u r@ timer8-ocr-a&b i!
    rdrop
  does>
    i@
;

: timer8-comp-a! ( u timer -- )
    timer8-ocr-a&b i@ u>2c drop c!
;

: timer8-comp-b! ( u timer -- )
    timer8-ocr-a&b i@ u>2c nip c!
;

: timer8-settings ( prescaler waveform pin-a pin-b -- control )
    4 lshift         \ pin-b mode (2 bits)
    swap 6 lshift or ( prescaler waveform pin-a+b ) \ pin-a mode (2 bits)
    over %11 and or ( prescaler waveform reg-a )
    swap %100 and 1 lshift ( prescaler reg-a waveform' )
    rot or          ( reg-a reg-b )
    2c>u
;

: +timer8 ( settings timer -- )
    timer8-cr-a&b i@ u>2c ( settings cr-a cr-b )
    rot u>2c ( cr-a cr-b a b )
    rot c! ( cr-a a )
    swap c!
;

: timer8-pin-a@ ( timer -- pinmask portaddr )
    timer8-pin-a i@ u>2c
;

: timer8-isr@ ( isr-id timer -- isr )
    timer8-isrs + i@
;

: timer8-imsk@ ( timer -- imsk )
    timer8-imsk&ifr i@ u>2c drop
;

0 constant TIMER8-OVF
1 constant TIMER8-COMP-A
2 constant TIMER8-COMP-B

: timer8-int! ( xt isr-id timer -- )
    timer8-isr@ int!
;

: +timer8-int ( isr-id timer -- )
    1 rot lshift swap timer8-imsk@ port-or!
;

: -timer8-int ( isr-id timer -- )
    1 rot lshift invert swap timer8-imsk@ port-and!
;
