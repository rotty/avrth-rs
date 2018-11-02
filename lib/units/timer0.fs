require struct.fs
require pins.fs
require timer8.fs

\ constants for timer0
device-address OCR0A device-address OCR0B
device-address TCCR0A device-address TCCR0B
device-address TCNT0
device-address TIMSK0 device-address TIFR0
device-interrupt TIMER0_OVF
device-interrupt TIMER0_COMPA
device-interrupt TIMER0_COMPB
device-address PORTD 6 device-address PORTD 5
create-timer8 timer0
