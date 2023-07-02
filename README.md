# wNES: Web NES Emulator

## Overview

wNES is an implementation of an NES (Nintendo Entertainment System) emulator.

NES emulators, even those targetting the web like this one, are already plenty.
This is mainly a personal project to familiarize myself with emulators, as the
NES seems to be a very easy platform to emulate - while still having awesome
games!

wNES goals are:

  - to be able to run most NES games

  - to target both the Web - through WebAssembly - as well as running natively
    on most popular operating systems.

    The native application may only have very rudimentary UI - the emulator's
    front-end is not the development area that attracts me the most and I'm
    not too familiar with Rust GUI development for now.

  - to have some nice-to-have emulator features:
      - save states
      - execution pauses with visual peeking of hardware state (CPU registers,
        memory, inputs...) and perhaps allowing its manipulation
      - and maybe more (execution time travel?), depending on the complexity of
        it.

On that matter, I started by following the following tutorial:
https://bugzmanov.github.io/nes_ebook
Though I try to not inspire myself too much of it, to get my real own
implementation of it (perhaps making emulator rookie mistakes along the way!).

## What's done for now

  - [x] CPU (6502 variant) emulation: Finished for the instruction
    implementing part.

    I will see about cycle accuracy and all that later.

  - [ ] Cartridge handling

  - [ ] PPU (Picture Processing Unit)

  - [ ] Inputs (Controllers)

  - [ ] APU (Audio Processing Unit)

  - [ ] Web port
