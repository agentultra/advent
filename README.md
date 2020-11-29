# Advent

My solutions for the annual [Advent of Code](https://adventofcode.com)
challenge.

Each year builds a new `advent<year>` executable that outputs the
solutions to each puzzle on `stdout`.

## Building ##

I use [stack](https://docs.haskellstack.org/en/stable/README/) as the
build tool.

### Starting a New Challenge ###

I wrote a script to generate all of the code needed to setup a
challenge for a new year:

    $ make year=2020

Replace `year=<your-year-here>` to generate the code for a new
challenge.

## Structure ##

The library is structured so that each solution is its own module, eg:

    src/Advent/Y2020/Day1/Part1.hs

Each module defines a `solution :: IO ()` function that prints the
answer to `stdout`.

Shared code goes into the level under `Advent` as appropriate.

Each module gets a corresponding test module under `test/`.

The solution is built into an executable to conveniently print out all
the answers under, eg: `app/Y2020/Main.hs`
