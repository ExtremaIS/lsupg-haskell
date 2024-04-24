# `lsupg-haskell` `0.4.0.0` Release Notes

Date
: 2024-04-25

## Overview

`lsupg` is a utility for listing items in a container that can be upgraded.
This information can be used to determine when a container image needs to be
updated.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/lsupg-haskell#lsupg>

## This Release

This release extends compatibility to support [GHC][] versions 8.6 through 9.8
for static as well as dynamic builds.  Both lower and upper bounds of
dependencies are now tested in CI.  Static builds are now done using a project
container, using [Alpine Linux][] and [GHCup][].  Both [Cabal][] and [Stack][]
are still supported.

[GHC]: <https://www.haskell.org/ghc/>
[Alpine Linux]: <https://www.alpinelinux.org/>
[GHCup]: <https://www.haskell.org/ghcup/>
[Cabal]: <https://www.haskell.org/cabal/>
[Stack]: <https://www.haskellstack.org>

There are no changes to the API or CLI.  The only change to the code is the
addition of build information to `--version` output when the executable is
built using [Alpine Linux][].
