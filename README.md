# avrth-rs

A Rust port of [avrth](https://r0tty.org/git/scheme/avrth/), a Scheme
implementation of Forth for the AVR microcontroller family, which in
turn is based on [AmForth](http://amforth.sourceforge.net/).

If you want a production-ready Forth implementation for AVR
microcontrollers, you are advised to use AmForth -- it comes with
documentation, a community, and is well-maintained.

This knock-off (and the original Scheme knock-off) tries to improve
upon AmForth mainly by including a Forth VM that can run on the host
system, but mimics the target VM as closely as possible.

This should allow you (eventually, see feature list at the bottom) to:

- Do some prototyping and testing without actually needing a
  microcontroller.

- Flash only the minimal runtime required to the target, leaving out
  the compiler and REPL, thus minimizing flash usage.

- Avoid overwriting the original boot loader on the Arduino
  platform. AmForth requires you to overwrite the boot loader, as only
  code running in the boot loader area can write to the flash
  memory. Avrth gives you the option of compiling a complete image on
  the host, which can be flashed just like you would when using C,
  leaving the bootloader area alone.

- Avrth aims to eventually implement a "mixed-mode" operation, where a
  part of the code runs on the host and part of it runs on the target,
  allowing prototyping on the host, while still being able to invoke
  code on the target seamlessly. Once you are satisfied with the
  newly-written code, you should be able to transfer it to the target.

Beside these directly user-facing benefits, having the option of
executing on the host also allows for some implementation-level
benefits:

- Non-primitive words in Avrth can be written in Forth, instead of
  AmForth's approach of "Assembler-that-is-really-Forth". The Avrth
  implementation contains a basic reader and compiler implemented in
  Rust which understands enough Forth syntax to read and compile the
  words constituting the actual Forth REPL.

- The Scheme implementation of Avrth includes an AVR assembler, so you
  don't need Wine and the proprietary assembler required to build
  AmForth. The Rust port does not yet include an assembler (nor any
  target-specific code), but the intention is to include one, perhaps
  even supporting the same syntax as the proprietary assembler needed
  to build AmForth, so the assembly code used in AmForth can be
  re-used directly.

## Features

This is the list of features working (or mostly-working ;-) in the
Scheme implementation, and their current status in the Rust port:

- [x] Host VM
- [x] Minimal reader and compiler for bootstrapping
- [x] Reader and compiler implemented in Forth
- [ ] Enhanced host-only development support (e.g. `require`)
- [ ] Assembler for AVR
- [ ] Compilation to ELF images
- [ ] Mixed-mode operation
