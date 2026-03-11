import SwiftUI

struct BriefingButton: View {
    let title: String
    let isRunning: Bool
    let action: () -> Void

    @State private var isHovering = false

    var body: some View {
        Button(action: action) {
            HStack {
                Text(isRunning ? "\(title)..." : title)
                Spacer()
                if isRunning {
                    ProgressView()
                        .controlSize(.small)
                }
            }
            .frame(maxWidth: .infinity, alignment: .leading)
        }
        .buttonStyle(.plain)
        .disabled(isRunning)
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
        .background(isHovering && !isRunning ? Color.accentColor.opacity(0.2) : Color.clear)
        .cornerRadius(4)
        .onHover { isHovering = $0 }
    }
}

struct MenuButton: View {
    let title: String
    let action: () -> Void

    init(_ title: String, action: @escaping () -> Void) {
        self.title = title
        self.action = action
    }

    @State private var isHovering = false

    var body: some View {
        Button(action: action) {
            Text(title)
                .frame(maxWidth: .infinity, alignment: .leading)
        }
        .buttonStyle(.plain)
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
        .background(isHovering ? Color.accentColor.opacity(0.2) : Color.clear)
        .cornerRadius(4)
        .onHover { isHovering = $0 }
    }
}

@main
struct CoopApp: App {
    @State private var manager = CoopManager()
    @State private var logStore = LogStore(url: CoopManager.logFile)
    @Environment(\.openWindow) private var openWindow

    var body: some Scene {
        Window("Coop Log", id: "log") {
            LogViewer(logStore: logStore)
        }
        .defaultLaunchBehavior(.suppressed)

        MenuBarExtra {
            VStack(alignment: .leading, spacing: 0) {
                Label(manager.statusText, systemImage: manager.statusIconName)
                    .padding(.horizontal, 12)
                    .padding(.vertical, 8)

                Divider()

                if manager.isRunning {
                    MenuButton("Restart") { manager.restart() }
                    MenuButton("Stop") { manager.stop() }
                } else {
                    MenuButton("Start") { manager.start() }
                }

                Divider()

                MenuButton("Config...") { manager.openConfig() }
                MenuButton("Log...") {
                    NSApp.activate()
                    openWindow(id: "log")
                }

                Divider()

                BriefingButton(
                    title: "Run Daily Briefing",
                    isRunning: manager.isDailyBriefingRunning
                ) { manager.runDailyBriefing() }
                BriefingButton(
                    title: "Run Weekly Briefing",
                    isRunning: manager.isWeeklyBriefingRunning
                ) { manager.runWeeklyBriefing() }

                Divider()

                Toggle("Launch at Login", isOn: Binding(
                    get: { manager.launchAtLogin },
                    set: { manager.setLaunchAtLogin($0) }
                ))
                .toggleStyle(.switch)
                .padding(.horizontal, 12)
                .padding(.vertical, 6)

                Divider()

                MenuButton("Quit ⌘Q") {
                    manager.stopSync()
                    NSApplication.shared.terminate(nil)
                }
            }
            .frame(width: 220)
        } label: {
            Image(manager.menuBarIconName)
        }
        .menuBarExtraStyle(.window)
    }
}
