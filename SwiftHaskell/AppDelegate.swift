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
        var argc = CommandLine.argc
        var argv = Optional.some(CommandLine.unsafeArgv)
        hs_init(&argc, &argv)

        label.stringValue = "\(square(5))"
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        hs_exit()
    }
}
