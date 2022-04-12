# `lsupg-haskell` `0.3.0.2` Release Notes

Date
: 2022-04-12

## Overview

This is a patch release of `lsupg`, updating package infrastructure and
bumping the upper bound of the following dependency.

* [ttc](https://hackage.haskell.org/package/ttc)

Static executables are now built using [ghc-musl][], using either [Cabal][] or
[Stack][].

[ghc-musl]: <https://github.com/utdemir/ghc-musl>
[Cabal]: <https://www.haskell.org/cabal/>
[Stack]: <https://www.haskellstack.org>

There are no changes to the code in this release.
