# lsupg

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/lsupg-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/lsupg-haskell/actions)

* [Overview](#overview)
* [CLI](#cli)
    * [Requirements](#requirements)
    * [Installation](#installation)
        * [Installation From Tarball](#installation-from-tarball)
        * [Installation From Source](#installation-from-source)
    * [Usage](#usage)
        * [Components](#components)
        * [Output](#output)
        * [Exit Codes](#exit-codes)
        * [Examples](#examples)
* [Project](#project)
    * [Links](#links)
    * [Releases](#releases)
    * [Contribution](#contribution)
    * [License](#license)

## Overview

`lsupg` is a utility for listing items in a container that can be upgraded.
This information can be used to determine when a container image needs to be
updated.

This project is currently in **beta**.  Parsing of command output should be
monitored, so that issues can be identified and resolved.

## CLI

### Requirements

`lsupg` runs inside of Linux containers.  It is a static executable, so it
should work with any distribution, but it only checks for upgrades of the
implemented [components](#components).

Running `lsupg` outside of a container (such as with the `--docker` option)
only works with Linux.

### Installation

#### Installation From Tarball

Check the [Releases][] page for a tarball with a built static executable.

[Releases]: <https://github.com/ExtremaIS/lsupg-haskell/releases>

#### Installation From Source

`lsupg` can be built from source using [Stack][].  For example, you can build
the latest release as follows:

[Stack]: <https://www.haskellstack.org>

```
$ git clone https://github.com/ExtremaIS/lsupg-haskell.git
$ cd lsupg-haskell
$ make static
```

The `lsupg` static executable is placed in the `build` directory.

### Usage

```
Usage: lsupg [--debug] [-f|--format FORMAT] [--docker IMAGE] [COMPONENT ...]
  list items that can be upgraded

Available options:
  -h,--help                show help and exit
  --version                show version and exit
  --debug                  show debug output
  -f,--format FORMAT       output format (default: human)
  --docker IMAGE           Docker image
  --nix-path PATH          check Nix upgrades against PATH (default:
                           /etc/nix/packages.nix)
  COMPONENT ...            components (default: all)
```

#### Components

Currently, the following components are supported.

Component | Items
----------|----------------------
`apk`     | Alpine Linux packages
`apt`     | Debian packages
`dnf`     | Fedora packages
`nix`     | Nix packages
`pacman`  | Arch Linux packages

Upgrades to Nix packages must be done using attribute paths, specified in a
Nix configuration file.  The `/etc/nix/packages.nix` file is used by default,
but a custom file can be specified using the `--nix-path` option.  Note that
upgrading using `nix-env -u` is fundamentally broken and should never be done.

#### Output

Currently, the following output formats are supported:

Format  | Description
--------|------------
`human` | ASCII table
`csv`   | CSV format
`json`  | JSON format
`yaml`  | YAML format

The following information for each item is displayed:

Column (`human`/`csv`) | Key (`json`/`yaml`) | Description
-----------------------|---------------------|------------------------------
1                      | `component_name`    | `lsupg` component
2                      | `item_name`         | item name
3                      | `installed_version` | installed version
4                      | `available_version` | version available for upgrade

The installed and available versions are both optional and can be interpreted
as follows:

Installed Version | Available Version | Interpretation
------------------|-------------------|------------------
Shown             | Shown             | upgrade/downgrade
Shown             | Empty             | removal
Empty             | Shown             | new install

#### Exit Codes

The following exit codes are used:

Exit Code | Meaning
----------|-------------------------------
0         | no upgrades available
1         | program error
2         | program usage error
3         | one or more upgrades available

#### Examples

The `lsupg` static binary should be mounted and executed within a container.
The `--docker` option provides an easy way to do this with a Docker
container.  For example, the following checks for upgrades in a
[debian](https://hub.docker.com/_/debian) container:

```
$ ./lsupg --docker debian:buster
apt  base-files   10.3+deb10u9     10.3+deb10u10
apt  libgcrypt20  1.8.4-5          1.8.4-5+deb10u1
apt  libhogweed4  3.4.1-1          3.4.1-1+deb10u1
apt  libgnutls30  3.6.7-4+deb10u6  3.6.7-4+deb10u7
apt  liblz4-1     1.8.3-1          1.8.3-1+deb10u1
```

Alternatively, the program can be mounted and run within a container manually.
The following is equivalent to the above example:

```
$ docker run --rm -it \
  -u root \
  -v "$(pwd)/lsupg:/usr/local/bin/lsupg:ro" \
  debian:buster \
  /usr/local/bin/lsupg
apt  base-files   10.3+deb10u9     10.3+deb10u10
apt  libgcrypt20  1.8.4-5          1.8.4-5+deb10u1
apt  libhogweed4  3.4.1-1          3.4.1-1+deb10u1
apt  libgnutls30  3.6.7-4+deb10u6  3.6.7-4+deb10u7
apt  liblz4-1     1.8.3-1          1.8.3-1+deb10u1
```

Note that the mount is read-only and the program must be run as a user that
can execute package management commands (typically `root`).

Use the `--debug` option to see debugging information.  The output of all
package management commands is displayed, allowing you to check that the
parsing is done correctly.

## Project

### Links

* GitHub: <https://github.com/ExtremaIS/lsupg-haskell>

### Releases

All releases are tagged in the `main` branch.  Release tags are signed using
the
[`security@extrema.is` GPG key](http://keys.gnupg.net/pks/lookup?op=vindex&fingerprint=on&search=0x1D484E4B4705FADF).

### Contribution

Issues and feature requests are tracked on GitHub:
<https://github.com/ExtremaIS/hr-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

### License

This project is released under the
[MIT License](https://opensource.org/licenses/MIT) as specified in the
[`LICENSE`](LICENSE) file.
