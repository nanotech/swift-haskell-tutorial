#!/usr/bin/env bash
set -eu

EXECUTABLE_NAME=SwiftHaskell
DIST_DIR="$(stack path --dist-dir)"
GHC_VERSION="$(stack exec -- ghc --numeric-version)"
GHC_LIB_DIR="$(stack path --compiler-bin)/../lib/ghc-$GHC_VERSION"
STUB_BUILD_DIR="${DIST_DIR}/build/${EXECUTABLE_NAME}/${EXECUTABLE_NAME}-tmp"
STUB_MODULE_DIR="${EXECUTABLE_NAME}/include"
STUB_MODULE_MAP="${STUB_MODULE_DIR}/module.modulemap"

# Create a module map from the generated Haskell
# FFI export headers for importing into Swift.
mkdir -p "${STUB_MODULE_DIR}"
NL="
"
module_map="module ${EXECUTABLE_NAME} {${NL}"
for h in $(find "${STUB_BUILD_DIR}" -name '*.h'); do
    h_filename="${h/$STUB_BUILD_DIR\//}"
    cp "$h" "${STUB_MODULE_DIR}/"
    module_map="${module_map}    header \"${h_filename}\"${NL}"
done
module_map="${module_map}    export *${NL}"
module_map="${module_map}}"
echo "${module_map}" > "${STUB_MODULE_MAP}"

# Symlink to the current GHC's header directory from a more
# convenient place for Xcode to find.
mkdir -p build/ghc
ln -sf "${GHC_LIB_DIR}/include" build/ghc/

# Symlink to the Haskell executable for Xcode.
ln -sf "../${DIST_DIR}/build/${EXECUTABLE_NAME}/${EXECUTABLE_NAME}" build/
