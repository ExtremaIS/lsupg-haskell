# `lsupg-haskell` TODO

## Functionality

General:

* Audit/clean code
* Add error handling?

Components:

* `yum`?
    * Do not run if `dnf` already run?
    * `yum check-update`

## Tests

## Compatibility

* [`hashable`](https://hackage.haskell.org/package/hashable)
  `1.5.0.0` is blocked by
  [`async`](https://hackage.haskell.org/package/async), a dependency of
  [`typed-process`](https://hackage.haskell.org/package/typed-process).
* [`tasty`](https://hackage.haskell.org/package/tasty)
  `1.5.1` is deprecated.  See issue
  [#421](https://github.com/UnkindPartition/tasty/issues/421).

## Documentation

## Project
