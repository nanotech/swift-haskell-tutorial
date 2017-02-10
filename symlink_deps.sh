#!/bin/sh
set -eu

DIST_DIR="$(stack path --dist-dir)"

ln -sf ../"$DIST_DIR"/build/SwiftHaskell/SwiftHaskell build/
