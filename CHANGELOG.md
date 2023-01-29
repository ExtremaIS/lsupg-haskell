# `lsupg-haskell` Changelog

This project follows the [Haskell package versioning policy][PVP], with
versions in `A.B.C.D` format.  `A` may be incremented arbitrarily for
non-technical reasons, but [semantic versioning][SemVer] is otherwise
followed, where `A.B` is the major version, `C` is the minor version, and `D`
is the patch version.  Initial development uses versions `0.0.0.D`, for which
every version is considered breaking.

[PVP]: <https://pvp.haskell.org/>
[SemVer]: <https://semver.org/>

The format of this changelog is based on [Keep a Changelog][KaC], with the
following conventions:

* Level-two heading `Unreleased` is used to track changes that have not been
  released.
* Other level-two headings specify the release in `A.B.C.D (YYYY-MM-DD)`
  format, with newer versions above older versions.
* Level-three headings are used to categorize changes as follows:
    1. Breaking
    2. Non-Breaking
* Changes are listed in arbitrary order and present tense.

[KaC]: <https://keepachangelog.com/en/1.0.0/>

## Unreleased

### Non-Breaking

* Bump `aeson` dependency version upper bound
* Bump `hashable` dependency version upper bound
* Bump `template-haskell` dependency version upper bound
* Bump `transformers` dependency version upper bound
* Bump `vector` dependency version upper bound

## 0.3.0.2 (2022-04-12)

### Non-Breaking

* Bump `ttc` dependency version upper bound
* Configure static building with Cabal or Stack for all supported GHC versions

## 0.3.0.1 (2022-02-28)

### Non-Breaking

* Bump `text` dependency version upper bound
* Bump `hashable` dependency version upper bound
* Bump `optparse-applicative` dependency version upper bound

## 0.3.0.0 (2021-07-07)

### Breaking

* Rewrite Nix component to use `/etc/nix/packages.nix`
* Add `--nix-path` option to support different configuration locations

### Non-Breaking

* Use TTC 1.1.0.1

## 0.2.0.0 (2021-06-25)

### Breaking

* Fix `--help` when using `optparse-applicative` `0.16`

### Non-Breaking

* Refactor Nix configuration

## 0.1.0.0 (2021-06-14)

### Breaking

* Initial public release
