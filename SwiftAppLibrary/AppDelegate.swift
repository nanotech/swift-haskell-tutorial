//
//  AppDelegate.swift
//  SwiftAppLibrary
//
//  Created by NanoTech on 2017-02-09.
//  Copyright © 2017 nanotech. All rights reserved.
//

import Cocoa

class AppDelegate: NSObject, NSApplicationDelegate {

    @IBOutlet weak var window: NSWindow!
    @IBOutlet weak var label: NSTextField!

    static var square: (@convention(c) (CInt) -> CInt)! = nil

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        label.stringValue = "\(AppDelegate.square(5))"
    }

    func applicationWillTerminate(_ aNotification: Notification) {
    }
}

@_cdecl("swiftAppMain")
func swiftAppMain(square: @escaping @convention(c) (CInt) -> CInt) {
    AppDelegate.square = square
    let app = NSApplication.shared()
    var topObjects: NSArray = []
    NSNib.init(nibNamed: "MainMenu", bundle: Bundle(for: AppDelegate.self))!
        .instantiate(withOwner: app, topLevelObjects: &topObjects)
    app.run()
}
