# Integrating Haskell with Swift Mac Apps

To start, create a new Cocoa Application Xcode project
with Swift as the default language:

![Select the Cocoa app template](tutorial/xcode-cocoa-template.png)

![Create a new project](tutorial/xcode-create-project.png)

Then `cd` into the directory with the `.xcodeproj` and create a
new stack project:

```sh
$ cd SwiftHaskell
$ stack new SwiftHaskellLibrary simple
```

Move these files up to the top directory, so we can run all of
our commands from the same directory:

```sh
$ mv -vn SwiftHaskellLibrary/* .
$ rmdir SwiftHaskellLibrary
```

In `SwiftHaskellLibrary.cabal`, rename the executable to
match the Xcode app's name of SwiftHaskell:

```cabal
executable SwiftHaskell
```

To combine our Haskell library with our Swift UI, we'll build
the Swift app as a framework and link to it from the Haskell
executable. Xcode will then package both up into an app bundle.

The reason for doing the linking in this direction is that
building a self-contained dynamic library is currently simpler
with Swift and Xcode than it is with Cabal.

## Exporting Haskell Functions

Here's the trivial function `square` that we'll export as a
simple first example:

```haskell
square x = x * x
```

Haskell functions exported via the FFI can only contain
certain types in their signatures that are compatible with C:
primitive integers, floats and doubles, and pointer types.
The full list is in [section 8.7 of the Haskell
Report][haskell-report-8.7].

Since we'll only be using `square` to demonstrate the FFI, let's
assign it a FFI-compatible type directly. For more complex
functions, wrap them in a new function and convert their inputs
and outputs as needed.

```haskell
import Foreign.C

square :: CInt -> CInt
square x = x * x
```

To export `square`, add a `foreign export` definition with a
calling convention of `ccall`:

```haskell
foreign export ccall square :: CInt -> CInt
```

For the full syntax of `foreign export`, see [section 8.3 of the
Haskell Report][haskell-report-8.3].

[haskell-report-8.7]: https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1700008.7
[haskell-report-8.3]: https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1530008.3

Together, `src/Main.hs` is

```haskell
module Main where

import Foreign.C

foreign export ccall square :: CInt -> CInt

square :: CInt -> CInt
square x = x * x

main :: IO ()
main = do
  putStrLn "hello world"
```

## Importing Haskell's Generated FFI Headers into Swift

If we now `stack build`, in addition to building the library,
GHC will generate C header files for each module with foreign
exports. Because these are build artifacts, they're buried
somewhat deep in the file hierarchy, but we can ask `stack`
and `find` where they are:

    $ find "$(stack path --dist-dir)" -name Main_stub.h
    .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/build/SwiftHaskellLibrary/SwiftHaskellLibrary-tmp/Main_stub.h

These stub headers `#include "HsFFI.h"` from GHC, so we'll also
need to find the current compiler's version of that header.

    $ find "$(stack path --compiler-bin)/.." -name HsFFI.h
    /Users/nanotech/.stack/programs/x86_64-osx/ghc-8.0.1/bin/../lib/ghc-8.0.1/include/HsFFI.h

Since we'll be importing these headers into a Swift framework,
we won't be able to use `#include` as we would in C. Instead,
Swift uses [Clang's module format][clang-modules]. (Swift
applications can use [bridging headers][swift-bridging-headers],
but frameworks [must use modules][so-non-modular-header].) A
`module.modulemap` file to import `Main_stub.h` looks like

    module SwiftHaskell {
        header "Main_stub.h"
        export *
    }

[swift-bridging-headers]: https://developer.apple.com/library/content/documentation/Swift/Conceptual/BuildingCocoaApps/MixandMatch.html#//apple_ref/doc/uid/TP40014216-CH10-ID156
[so-non-modular-header]: https://stackoverflow.com/questions/24103169/swift-compiler-error-non-modular-header-inside-framework-module/37072619#37072619
[clang-modules]: http://clang.llvm.org/docs/Modules.html

As the paths to these headers vary, let's use a script to
automatically copy them out and build a module map. We'll also
create a symlink to the built executable's location for later.

```bash
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
```

Save the script as `link-deps.sh`, run `stack build`, and then
run `bash link-deps.sh` to prepare for the next section.

## Converting the Swift App to a Framework

Create a new Cocoa Framework target in the Xcode project
named SwiftAppLibrary, then change the Target Membership of
`AppDelegate.swift` and `MainMenu.xib` to only SwiftAppLibrary
in Xcode's File Inspector in the right sidebar:

![Target Membership](tutorial/xcode-target-membership.png)

In the new framework's build settings, set **Always Embed Swift
Standard Libraries** to **Yes**.

Drag the `SwiftHaskell` executable we built previously with
Stack into Xcode from the `build/` directory that we symlinked
it into, but do not add it to any targets when prompted:

![The SwiftHaskell executable in Xcode](tutorial/xcode-files-swifthaskell-executable.png)

In the SwiftHaskell app target's Build Phases, remove the
**Compile Sources** and **Link Binary With Libraries**
phases, and add a new **Copy Files** phase that copies the
`SwiftHaskell` executable into the app bundle's Executables
directory:

![Copy into Executables](tutorial/xcode-copy-files-swifthaskell-executable.png)

Finally, in the SwiftAppLibrary framework target's Build Phases,
add a new **Run Script** phase to create a symlink to the built
framework for us to link to from Cabal:

```sh
set -u
ln -sf "${BUILT_PRODUCTS_DIR}/${FULL_PRODUCT_NAME}" "${PROJECT_DIR}/build/"
```

## Linking to the Framework

Add these options to the executable's section in the `.cabal`
file:

```cabal
executable SwiftHaskell
  ghc-options:         -threaded -framework-path build
  ld-options:          -rpath @executable_path/../Frameworks
  frameworks:          SwiftAppLibrary
```

- `-threaded` enables the multithreaded GHC runtime, which is
   usually what you want.
- `-framework-path build` tells GHC to look for frameworks where we
  symlinked our framework to.
- `-rpath @executable_path/../Frameworks` embeds into the
  executable where the dynamic linker should look for shared
  libraries.

## Starting the GUI

Because Haskell has control over the program's entry point
(`main`), we'll need to have it call out to Swift to
start Cocoa's main thread. Add this top level function to
`AppDelegate.swift`:

```swift
@_cdecl("runNSApplication")
func runNSApplication() {
    let app = NSApplication.shared()
    var topObjects: NSArray = []
    NSNib.init(nibNamed: "MainMenu", bundle: Bundle(for: AppDelegate.self))!
        .instantiate(withOwner: app, topLevelObjects: &topObjects)
    app.run()
}
```

`@_cdecl` sets the symbol name of the Swift function, overriding
normal symbol mangling.

In `SwiftAppLibrary.h`, add a `FOUNDATION_EXPORT` C prototype
for the function, so that its symbol gets marked as public and
is exported from the framework:

```c
FOUNDATION_EXPORT void runNSApplication(void);
```

In `Main.hs`, import the foreign function and call it from
the end of `main`:

```haskell
module Main where

import Foreign.C

foreign export ccall square :: CInt -> CInt

square :: CInt -> CInt
square x = x * x

foreign import ccall "runNSApplication" runNSApplication :: IO ()

main :: IO ()
main = do
  putStrLn "hello world"
  runNSApplication
```

`runNSApplication` will not return, being busy with Cocoa's
main run loop. Use `Control.Concurrent.forkIO` before calling
`runNSApplication` to run other tasks as needed.

Run `stack build`, and build and run the `SwiftHaskell` app
target in Xcode to launch the app and see the default window
from `MainMenu.xib`:

![A blank window](tutorial/empty-app.png)

## Completing Linking Setup

Add `$(PROJECT_DIR)/SwiftHaskell/include` to the framework
target's **Swift Compiler - Search Paths, Import Paths** setting
in Xcode,

![The Swift module import paths](tutorial/xcode-swift-module-search-paths.png)

and `$(PROJECT_DIR)/build/ghc/include` to the framework's **User
Header Search Paths** setting:

![The User Header Search Paths](tutorial/xcode-header-search-paths.png)

In order for the framework to be able to link to symbols in the
Haskell executable, we need to tell the linker to leave symbols
undefined and have them be resolved at runtime.

Add `-undefined dynamic_lookup` to the framework's **Other
Linker Flags** setting.

Be aware that this means that link errors will occur at runtime
instead of at link time. Also note that the framework linking
to symbols in the executable (and depending on the generated
headers), and the executable linking to the framework, creates
a circular dependency. When initially building the project, you
will need to build the components in this order:

- `stack build` to generate the Haskell FFI export headers.
  Linking will fail, as the Swift framework is not built yet.
- Build the Swift framework.
- `stack build`
- Build the app bundle.

The first step can be skipped subsequently by committing the
generated headers to source control.

To help automate this, add a new **Run Script** build phase to
the beginning of the framework's build phases with the contents

```sh
stack build
bash link-deps.sh
```

Add the Haskell sources as input files:

```
$(PROJECT_DIR)/src/Main.hs
$(PROJECT_DIR)/SwiftHaskellLibrary.cabal
```

And the executable as an output file:

```
$(PROJECT_DIR)/build/SwiftHaskell
```

TODO: `xcodebuild` from `Setup.hs` hooks?

## Calling Haskell from Swift

We're now ready to use exported Haskell functions from Swift.
Import `SwiftHaskell` at the top of `AppDelegate.swift`

```swift
import SwiftHaskell
```

Add a new label to the window in `MainMenu.xib` for us to write
the result of our Haskell function `square` into, and add it as
an `@IBOutlet` to the `AppDelegate`:

```swift
@IBOutlet weak var label: NSTextField!
```

We already have our Haskell library's header imported, so we
can just call the exported `square` function. Add this to
`applicationDidFinishLaunching`:

```swift
label.stringValue = "\(square(5))"
```

The final contents of `AppDelegate.swift` are:

```swift
import Cocoa
import SwiftHaskell

class AppDelegate: NSObject, NSApplicationDelegate {

    @IBOutlet weak var window: NSWindow!
    @IBOutlet weak var label: NSTextField!

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        label.stringValue = "\(square(5))"
    }

    func applicationWillTerminate(_ aNotification: Notification) {
    }
}

@_cdecl("swiftAppMain")
func swiftAppMain() {
    let app = NSApplication.shared()
    var topObjects: NSArray = []
    NSNib.init(nibNamed: "MainMenu", bundle: Bundle(for: AppDelegate.self))!
        .instantiate(withOwner: app, topLevelObjects: &topObjects)
    app.run()
}
```

Running the app,

![5 squared](tutorial/squared.png)

## Strings and ByteString FFI

TODO

## Function and Closure FFI

TODO

