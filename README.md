# Advent of Code solution archive

This is my complete set of solutions for the [Advent of Code](https://adventofcode.com)
annual programming game.

Generated Haddocks are available at <https://glguy.net/advent/>

These libraries and solutions are provided under the ISC license.

## Build steps

1. Install [GHCUP](https://www.haskell.org/ghcup/)
2. Install current version of GHC `ghcup install ghc 9.4.4`
3. Update submodules `git submodule update --init`
4. Configure to use correct GHC `cabal configure -w ghc-9.4.4`
4. Build everything `cabal build all`

GHC 9.4.4 isn't specifically required, however it's what I
test with and what I use in CI.

## Running solutions

There are multiple methods for finding the input file for
each solution.

1. Default (no command line argument) reads file `inputs/YEAR/DAY.txt`
2. Filename argument reads the given file.
3. Hyphen `-` argument reads from stdin.
4. Plus `+` arguent reads input from second command line argument as a string literal.

Examples:

```
$ sln_2022_01             # defaults to inputs/2022/01.txt
$ sln_2022_01 example.txt # reads example.txt
$ sln_2022_01 -           # reads from stdin
$ sln_2022_01 + '"1\n2\n\n3\n4\n"' # parses Haskell string literal
```
