# Advent of Code solution archive

This is my complete set of solutions for the [Advent of Code](https://adventofcode.com)
annual programming game.

Generated Haddocks are available at <https://glguy.net/advent/>

These libraries and solutions are provided under the ISC license.

## Build steps

1. Install [GHCUP](https://www.haskell.org/ghcup/)
2. Install current version of GHC `ghcup install ghc 9.4.2`
3. Update submodules `git submodule update --init`
4. Configure to use correct GHC `cabal configure -w ghc-9.4.2`
4. Build everything `cabal build all`

GHC 9.4.2 isn't specifically required, but at the time of writing this document
it's the most recent release that also has HLS support.
