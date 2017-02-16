//
//  AppDelegate.swift
//  SwiftAppLibrary
//
//  Created by NanoTech on 2017-02-09.
//  Copyright © 2017 nanotech. All rights reserved.
//

import Cocoa
import SwiftHaskell

class AppDelegate: NSObject, NSApplicationDelegate {

    @IBOutlet weak var window: NSWindow!
    @IBOutlet weak var label: NSTextField!

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        let bytes: [UInt8] = [1, 2, 3, 2, 1, 2]
        callbackExample { n in
            print(n)
        }
        var callbackResult: CInt? = nil
        contextCallbackExample { n in
            callbackResult = n
        }
        let m = Multiplier(5)
        label.stringValue = [
            "5^2 = \(square(5))",
            "2 occurs \(count(byte: 2, in: bytes)) times in \(bytes)",
            "getSequence returned \(getSequence())",
            "Context callback returned \(callbackResult)",
            "5 * 3 = \(m.multiply(3))",
        ].joined(separator: "\n")
    }

    func applicationWillTerminate(_ aNotification: Notification) {
    }
}

@_cdecl("runNSApplication")
func runNSApplication() {
    let app = NSApplication.shared()
    var topObjects: NSArray = []
    NSNib.init(nibNamed: "MainMenu", bundle: Bundle(for: AppDelegate.self))!
        .instantiate(withOwner: app, topLevelObjects: &topObjects)
    app.run()
}


// Swift to Haskell ByteString Example

func count(byte: UInt8, in bytes: [UInt8]) -> Int {
    var r = 0
    bytes.withUnsafeBufferPointer { bytesBufPtr in
        r = Int(SwiftHaskell.countBytes(byte,
                                        HsPtr(mutating: bytesBufPtr.baseAddress),
                                        HsWord64(bytesBufPtr.count)))
    }
    return r
}

// Haskell to Swift ByteString Example

func getSequence() -> [UInt8] {
    var n = 0
    let p = SwiftHaskell.getSequence(&n).assumingMemoryBound(to: UInt8.self)
    let a = [UInt8](UnsafeBufferPointer(start: p, count: n))
    free(p)
    return a
}

// Swift to Haskell Callback Example

func callbackExample(f: (@convention(c) (CInt) -> Void)) {
    let hsf = unsafeBitCast(f, to: HsFunPtr.self)
    SwiftHaskell.callbackExample(hsf)
}

// Swift to Haskell Callback with Context Example

func contextCallbackExample(f: ((CInt) -> Void)) {
    class Wrap<T> {
        var inner: T

        init(_ inner: T) {
            self.inner = inner
        }
    }
    let x = 3
    func release(context: HsPtr) {
        let _: Wrap<(CInt) -> Void> = Unmanaged.fromOpaque(context).takeRetainedValue()
    }
    func call(context: HsPtr, value: CInt) {
        let wf: Wrap<(CInt) -> Void> = Unmanaged.fromOpaque(context).takeUnretainedValue()
        let f = wf.inner
        f(value)
    }
    let release_hs = unsafeBitCast(
        release as @convention(c) (HsPtr) -> Void, to: HsFunPtr.self)
    let call_hs = unsafeBitCast(
        call as @convention(c) (HsPtr, CInt) -> Void, to: HsFunPtr.self)
    let ctx = Unmanaged.passRetained(Wrap(f)).toOpaque()
    SwiftHaskell.contextCallbackExample(ctx, release_hs, call_hs)
}

// Swift to Haskell Function Example

class Multiplier {
    let funPtr: HsFunPtr

    init(_ x: CInt) {
        self.funPtr = SwiftHaskell.makeMultiplier(x)
    }

    func multiply(_ y: CInt) -> CInt {
        typealias F = @convention(c) (CInt) -> CInt
        let f = unsafeBitCast(self.funPtr, to: F.self)
        return f(y)
    }

    deinit {
        SwiftHaskell.freeMultiplier(self.funPtr)
    }
}
