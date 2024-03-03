# wNES: Web NES Emulator

## Overview

wNES is an implementation of an NES (Nintendo Entertainment System) emulator.

NES emulators, even those targetting the web like this one, are already plenty.
This is mainly a personal project to familiarize myself with emulators, as the
NES seems to be a very easy platform to emulate - while still having awesome
games!

wNES goals are:

-   to be able to run most NES games

-   to target both the Web - through WebAssembly - as well as running natively
    on most popular operating systems.

    The native application may only have very rudimentary UI - the emulator's
    front-end is not the development area that attracts me the most and I'm
    not too familiar with Rust GUI development for now.

-   to have some nice-to-have emulator features:
    -   save states
    -   execution pauses with visual peeking of hardware state (CPU registers,
        memory, inputs...) and perhaps allowing its manipulation
    -   and maybe more (execution time travel?), depending on the complexity of
        it.

## What's done for now

It can run games which do not involve mappers, which are modules allowing to
implement capabilities not found inside a NES console but through supplementary
hardware integrated inside game cartridges instead.

Yet, many early and/or simple games which do not rely on any mapper, such as
pacman and the first Super Mario, are runnable.

However even for those supported games, there are many rough edges, especially:

-   timing between the PPU (the component handling the rendering) and the CPU is
    not perfect.

    For example Super Mario does a trick where it bases itself on some PPU
    interrupt to keep the user's score on screen but to scroll the rest of the
    background.

    Here, this trick does happen but is short a few lines. I don't know why yet,
    I guess one of my timing calculation is off.

-   Audio is not implemented yet. It didn't sound to be a fun part as much to me
    for now!

-   Many PPU registers and tricks, such as those relied on to hide sprites or
    background are not implemented yet.

    To re-use the Super Mario example, mushrooms appear in front of a pushed box
    as soon as Mario interacted with it, whereas in the original game they
    appear to go out of the box and are thus behind it.
