import SwiftUI

@main
struct CoopApp: App {
    @State private var manager = CoopManager()

    var body: some Scene {
        MenuBarExtra {
            Label(manager.statusText, systemImage: manager.statusIconName)

            Divider()

            if manager.isRunning {
                Button("Restart") { manager.restart() }
                Button("Stop") { manager.stop() }
            } else {
                Button("Start") { manager.start() }
            }

            Divider()

            Button("Config...") { manager.openConfig() }
            Button("Log...") { manager.openLog() }

            Divider()

            Toggle("Launch at Login", isOn: Binding(
                get: { manager.launchAtLogin },
                set: { manager.setLaunchAtLogin($0) }
            ))

            Divider()

            Button("Quit") {
                manager.stopSync()
                NSApplication.shared.terminate(nil)
            }
            .keyboardShortcut("q")
        } label: {
            Image(manager.menuBarIconName)
        }
        .menuBarExtraStyle(.menu)
    }
}
