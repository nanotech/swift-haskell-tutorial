#!/bin/sh

set -eux

DIST_DIR="$(stack path --dist-dir)"
GHC_VERSION="$(stack exec -- ghc --numeric-version)"
GHC_LIB_DIR="$(stack path --compiler-bin)/../lib/ghc-$GHC_VERSION"

ln -sf ../"$DIST_DIR"/build/Lib_stub.h build/
ln -sf "$GHC_LIB_DIR"/include build/
ln -sf "$GHC_LIB_DIR"/rts/libHSrts_thr-ghc"$GHC_VERSION".dylib build/
ln -sf "$GHC_LIB_DIR"/rts/libffi.dylib build/
