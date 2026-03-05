import AppKit
import Foundation
import Observation
import ServiceManagement

@Observable
final class CoopManager {

    // MARK: - Types

    enum Status {
        case stopped
        case running
        case error(String)
    }

    // MARK: - State

    private(set) var status: Status = .stopped
    private(set) var launchAtLogin: Bool = false

    // MARK: - Private

    private var process: Process?
    private var retryCount = 0

    private var terminationObserver: Any?

    private let maxRetries = 5

    // MARK: - Paths

    static let configDir: URL = FileManager.default.homeDirectoryForCurrentUser
        .appendingPathComponent(".config/coop")
    static let configFile = configDir.appendingPathComponent("coop-local.dhall")
    static let logFile = configDir.appendingPathComponent("coop.log")

    // MARK: - Computed

    var isRunning: Bool {
        if case .running = status { return true }
        return false
    }

    var statusText: String {
        switch status {
        case .stopped: "Stopped"
        case .running: "Running"
        case .error(let msg): "Error: \(msg)"
        }
    }

    var statusIconName: String {
        switch status {
        case .stopped: "circle"
        case .running: "circle.fill"
        case .error: "exclamationmark.circle.fill"
        }
    }

    var menuBarIconName: String {
        switch status {
        case .stopped: "bird"
        case .running: "bird.fill"
        case .error: "exclamationmark.triangle"
        }
    }

    // MARK: - Init

    init() {
        launchAtLogin = SMAppService.mainApp.status == .enabled

        terminationObserver = NotificationCenter.default.addObserver(
            forName: NSApplication.willTerminateNotification,
            object: nil,
            queue: .main
        ) { [weak self] _ in
            self?.stopSync()
        }
    }

    deinit {
        if let observer = terminationObserver {
            NotificationCenter.default.removeObserver(observer)
        }
    }

    // MARK: - Process Control

    func start() {
        retryCount = 0
        launchProcess()
    }

    func stop() {
        guard let process, process.isRunning else {
            status = .stopped
            self.process = nil
            return
        }
        process.terminationHandler = nil
        process.terminate()

        DispatchQueue.global().async { [weak self] in
            let deadline = Date().addingTimeInterval(3)
            while process.isRunning && Date() < deadline {
                Thread.sleep(forTimeInterval: 0.1)
            }
            if process.isRunning {
                kill(process.processIdentifier, SIGKILL)
            }
            DispatchQueue.main.async {
                self?.process = nil
                self?.status = .stopped

            }
        }
    }

    func restart() {
        guard let process, process.isRunning else {
            start()
            return
        }
        process.terminationHandler = nil
        process.terminate()

        DispatchQueue.global().async { [weak self] in
            let deadline = Date().addingTimeInterval(3)
            while process.isRunning && Date() < deadline {
                Thread.sleep(forTimeInterval: 0.1)
            }
            if process.isRunning {
                kill(process.processIdentifier, SIGKILL)
            }
            DispatchQueue.main.async {
                self?.process = nil

                self?.start()
            }
        }
    }

    func stopSync() {
        guard let process, process.isRunning else { return }
        process.terminationHandler = nil
        process.terminate()

        let deadline = Date().addingTimeInterval(3)
        while process.isRunning && Date() < deadline {
            Thread.sleep(forTimeInterval: 0.1)
        }
        if process.isRunning {
            kill(process.processIdentifier, SIGKILL)
        }
        self.process = nil
    }

    // MARK: - Config & Log Actions

    func openConfig() {
        ensureConfigDir()

        if !FileManager.default.fileExists(atPath: Self.configFile.path) {
            try? Self.defaultConfigTemplate.write(
                to: Self.configFile,
                atomically: true,
                encoding: .utf8
            )
        }
        openInTextEditor(Self.configFile)
    }

    func openLog() {
        ensureConfigDir()

        if !FileManager.default.fileExists(atPath: Self.logFile.path) {
            FileManager.default.createFile(atPath: Self.logFile.path, contents: nil)
        }
        openInTextEditor(Self.logFile)
    }

    private func openInTextEditor(_ url: URL) {
        let textEdit = URL(fileURLWithPath: "/System/Applications/TextEdit.app")
        NSWorkspace.shared.open(
            [url],
            withApplicationAt: textEdit,
            configuration: NSWorkspace.OpenConfiguration()
        )
    }

    // MARK: - Launch at Login

    func setLaunchAtLogin(_ enabled: Bool) {
        do {
            if enabled {
                try SMAppService.mainApp.register()
            } else {
                try SMAppService.mainApp.unregister()
            }
            launchAtLogin = enabled
        } catch {
            launchAtLogin = SMAppService.mainApp.status == .enabled
        }
    }

    // MARK: - Private

    private func launchProcess() {
        guard let coopPath = resolveCoopPath() else {
            status = .error("coop not found in PATH")
            return
        }

        ensureConfigDir()
        writeLogSeparator()

        let cmd =
            "exec '\(coopPath)' --config '\(Self.configFile.path)' >> '\(Self.logFile.path)' 2>&1"

        let proc = Process()
        let shell = ProcessInfo.processInfo.environment["SHELL"] ?? "/bin/zsh"
        proc.executableURL = URL(fileURLWithPath: shell)
        proc.arguments = ["-l", "-c", cmd]

        proc.terminationHandler = { [weak self] terminatedProcess in

            DispatchQueue.main.async {
                self?.handleTermination(terminatedProcess)
            }
        }

        do {
            try proc.run()
            process = proc
            status = .running

        } catch {

            status = .error(error.localizedDescription)
        }
    }

    private func handleTermination(_ proc: Process) {
        process = nil

        guard proc.terminationStatus != 0 else {
            status = .stopped

            return
        }

        if retryCount < maxRetries {
            let delay = pow(2.0, Double(retryCount))
            retryCount += 1
            status = .error("Retry \(retryCount)/\(maxRetries) in \(Int(delay))s")

            DispatchQueue.main.asyncAfter(deadline: .now() + delay) { [weak self] in
                self?.launchProcess()
            }
        } else {
            status = .error("Max retries reached")

        }
    }

    private func resolveCoopPath() -> String? {
        // Phase 2: check bundled binary
        let bundled = Bundle.main.bundleURL
            .appendingPathComponent("Contents/MacOS/coop")
        if FileManager.default.fileExists(atPath: bundled.path) {
            return bundled.path
        }

        // Check well-known paths
        let home = FileManager.default.homeDirectoryForCurrentUser.path
        let candidates = [
            "\(home)/.cabal/bin/coop",
            "\(home)/.local/bin/coop",
            "\(home)/.ghcup/bin/coop",
            "/opt/homebrew/bin/coop",
            "/usr/local/bin/coop",
        ]
        for path in candidates {
            if FileManager.default.fileExists(atPath: path) {
                return path
            }
        }

        // Fallback: resolve via user's login shell
        let proc = Process()
        let shell = ProcessInfo.processInfo.environment["SHELL"] ?? "/bin/zsh"
        proc.executableURL = URL(fileURLWithPath: shell)
        proc.arguments = ["-l", "-c", "which coop"]
        let pipe = Pipe()
        proc.standardOutput = pipe
        proc.standardError = FileHandle.nullDevice

        do {
            try proc.run()
            proc.waitUntilExit()
            if proc.terminationStatus == 0,
                let data = try pipe.fileHandleForReading.readToEnd(),
                let path = String(data: data, encoding: .utf8)?
                    .trimmingCharacters(in: .whitespacesAndNewlines),
                !path.isEmpty
            {
                return path
            }
        } catch {}

        return nil
    }

    private func writeLogSeparator() {
        let ts = ISO8601DateFormatter().string(from: Date())
        let separator = "\n--- coop started at \(ts) ---\n"
        if let data = separator.data(using: .utf8) {
            if FileManager.default.fileExists(atPath: Self.logFile.path) {
                if let handle = try? FileHandle(forWritingTo: Self.logFile) {
                    handle.seekToEndOfFile()
                    handle.write(data)
                    try? handle.close()
                }
            } else {
                FileManager.default.createFile(atPath: Self.logFile.path, contents: data)
            }
        }
    }

    private func ensureConfigDir() {
        try? FileManager.default.createDirectory(
            at: Self.configDir,
            withIntermediateDirectories: true
        )
    }

    // MARK: - Default Config

    static let defaultConfigTemplate = """
        -- coop local configuration
        -- Fill in your API keys and IDs below.
        -- See: config/coop-local.dhall.example in the coop repository

        let Mode = < Dryrun | Live >
        let ConnMode = < Webhook | SocketMode >

        in
        { cfgMode = Mode.Live
        , cfgPort = 3000
        , cfgLogLevel = "INFO"
        , cfgSlack =
          { slackBotToken = ""
          , slackSigningSecret = ""
          , slackBotUserId = ""
          , slackMonitoredUserId = ""
          , slackNotifyChannel = ""
          , slackAppToken = ""
          , slackConnectionMode = ConnMode.SocketMode
          , slackCatchupChannels = ""
          }
        , cfgLLM =
          { llmBackend = "Claude"
          , llmClaude =
            { claudeApiKey = ""
            , claudeModel = "claude-sonnet-4-20250514"
            }
          , llmOpenAI =
            { openaiApiKey = ""
            , openaiModel = "gpt-4o"
            }
          }
        , cfgNotion =
          { notionApiKey = ""
          , notionTaskDatabaseId = ""
          , notionGuidelinesPageId = ""
          , notionInstructionsPageId = ""
          , notionPropName = "Name"
          , notionPropPriority = "Priority"
          , notionPropStatus = "Status"
          , notionPropDueDate = ""
          , notionStatusOpen = "Open"
          , notionStatusInProgress = "In Progress"
          , notionStatusDone = [ "Done" ]
          , notionPropAssignee = ""
          , notionAssigneeUserId = ""
          , notionPropEstimate = ""
          }
        , cfgDryrun =
          { dryrunDataDir = "config/dryrun-data"
          }
        , cfgScheduler =
          { schedulerBriefingCron = "0 9 * * *"
          , schedulerWeeklyBriefingCron = "-"
          , schedulerWeeklyAvailableHours = 30
          }
        , cfgGoogleCalendar =
          { googleClientId = ""
          , googleClientSecret = ""
          , googleCalendarId = "primary"
          , googleTokenPath = "~/.config/coop/google-token.json"
          }
        }
        """
}
