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
        label.stringValue = "\(square(5))"
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
