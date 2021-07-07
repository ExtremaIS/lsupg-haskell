# `lsupg-haskell` `0.3.0.0` Release Notes

Date
: 2021-07-07

## Overview

This release of `lsupg` makes significant changes to the [Nix][] component.

[Nix]: <https://nixos.org/>

### Nix Packages Configuration

Previous versions of this software used `nix-env --upgrade --dry-run` to
determine what upgrades are available.  With no arguments, this command checks
all installed packages.  Unfortunately, it does so by package name, which does
not result in the desired behavior.  The general consensus is that the
`nix-env -u` is fundamentally broken and should never be used.

The correct way to manage package upgrades is to use attribute paths.  This
software now calculates available upgrades based on attribute paths listed in
a configuration file.  The `/etc/nix/packages.nix` file is used by default,
but a custom file can be specified using the `--nix-path` option.

### Bug Fix

During the rewrite of the Nix component, a bug was fixed so that package names
that start with a digit (such as `0ad`) are handled correctly.
