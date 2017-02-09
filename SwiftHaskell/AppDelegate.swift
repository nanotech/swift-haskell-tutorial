//
//  AppDelegate.swift
//  SwiftHaskell
//
//  Created by NanoTech on 2017-02-08.
//  Copyright © 2017 nanotech. All rights reserved.
//

import Cocoa

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {

    @IBOutlet weak var window: NSWindow!
    @IBOutlet weak var label: NSTextField!

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        var argv0 = Array("SwiftHaskell".utf8CString)
        argv0.withUnsafeMutableBufferPointer { argv0bp in
            var argv = [argv0bp.baseAddress];
            var argc = CInt(argv.count)
            argv.withUnsafeMutableBufferPointer { argvbp in
                var argvp = argvbp.baseAddress
                hs_init(&argc, &argvp)
            }
        }

        label.stringValue = "\(square(5))"
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        hs_exit()
    }
}
