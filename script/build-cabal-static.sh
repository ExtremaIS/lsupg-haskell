#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
#set -o xtrace

die () {
  echo "error: $*" >&2
  exit 2
}

cd "/host" || die "/host not found"

cabal v2-build --enable-executable-static "$@" || die "cabal: exit code $?"

PUID="$(stat -c '%u' /host)"
PGID="$(stat -c '%g' /host)"
chown -R "${PUID}:${PGID}" "dist-newstyle" || die "chown: exit code $?"
