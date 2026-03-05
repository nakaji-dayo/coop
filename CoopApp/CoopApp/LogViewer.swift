import SwiftUI
import Observation

@Observable
final class LogStore {
    var lines: [String] = []
    private var fileHandle: FileHandle?
    private var source: DispatchSourceFileSystemObject?
    private let url: URL

    init(url: URL) {
        self.url = url
    }

    func start() {
        loadExisting()
        watchFile()
    }

    func stop() {
        source?.cancel()
        source = nil
        try? fileHandle?.close()
        fileHandle = nil
    }

    func clear() {
        lines = []
    }

    private func loadExisting() {
        guard let data = try? Data(contentsOf: url),
              let text = String(data: data, encoding: .utf8)
        else { return }

        let tail = text.components(separatedBy: "\n").suffix(500)
        lines = Array(tail)
        openHandle(at: UInt64(data.count))
    }

    private func openHandle(at offset: UInt64) {
        guard FileManager.default.fileExists(atPath: url.path) else { return }
        let fh = FileHandle(forReadingAtPath: url.path)
        fh?.seek(toFileOffset: offset)
        fileHandle = fh
    }

    private func watchFile() {
        let fd = open(url.path, O_RDONLY | O_EVTONLY)
        guard fd >= 0 else { return }

        let src = DispatchSource.makeFileSystemObjectSource(
            fileDescriptor: fd,
            eventMask: [.write, .extend],
            queue: .main
        )
        src.setEventHandler { [weak self] in
            self?.readNew()
        }
        src.setCancelHandler {
            close(fd)
        }
        src.resume()
        source = src
    }

    private func readNew() {
        guard let fh = fileHandle else { return }
        let data = fh.availableData
        guard !data.isEmpty,
              let text = String(data: data, encoding: .utf8)
        else { return }

        let newLines = text.components(separatedBy: "\n")
        if let last = lines.last, !last.isEmpty, let first = newLines.first {
            lines[lines.count - 1] = last + first
            lines.append(contentsOf: newLines.dropFirst())
        } else {
            lines.append(contentsOf: newLines)
        }
    }
}

struct LogViewer: View {
    let logStore: LogStore

    var body: some View {
        ScrollViewReader { proxy in
            ScrollView {
                LazyVStack(alignment: .leading, spacing: 0) {
                    ForEach(Array(logStore.lines.enumerated()), id: \.offset) { index, line in
                        Text(line)
                            .font(.system(size: 11, design: .monospaced))
                            .textSelection(.enabled)
                            .frame(maxWidth: .infinity, alignment: .leading)
                            .id(index)
                    }
                }
                .padding(8)
            }
            .onChange(of: logStore.lines.count) { _, newCount in
                if newCount > 0 {
                    proxy.scrollTo(newCount - 1, anchor: .bottom)
                }
            }
        }
        .frame(minWidth: 600, minHeight: 400)
        .onAppear { logStore.start() }
        .onDisappear { logStore.stop() }
    }
}
