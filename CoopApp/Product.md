# coop-app — macOS メニューバーアプリ 仕様書

## 1. 概要

coop CLI（Haskell製）のmacOSメニューバー常駐アプリ。coopプロセスのデーモン管理（起動・停止・再起動）、設定ファイルの編集、ログ閲覧を提供する。

## 2. 技術スタック

| 項目 | 選定 | 備考 |
|------|------|------|
| フレームワーク | SwiftUI | `MenuBarExtra` (macOS 13+) を使用 |
| 最小対応OS | macOS 13 (Ventura) | `MenuBarExtra`, `SMAppService` の要件 |
| プロセス管理 | Foundation `Process` | coop CLIの起動・監視・停止 |
| 自動起動 | `SMAppService.mainApp` | ログイン時自動起動のトグル |
| coop バイナリ | `cabal install` 済み前提 | PATHから解決。将来的にアプリバンドル同梱を検討 |

## 3. アプリ構成

- Xcodeプロジェクト `coop-app` をリポジトリ直下に作成
- `Info.plist` で `Application is agent (UIElement) = YES` を設定（Dockアイコン非表示）
- メインウィンドウなし、`MenuBarExtra` のみのアプリ

## 4. メニューバー

### 4.1 アイコン

ステータスバーにSF Symbolsのアイコンを表示。coopの状態に応じてアイコンを切り替える:

| 状態 | アイコン | 説明 |
|------|----------|------|
| 停止中 | `bird` (グレー) | coopプロセスが動いていない |
| 起動中 | `bird.fill` (緑) | coopプロセスが正常稼働中 |
| エラー | `bird.fill` (赤) | 異常終了・再起動上限到達 |

### 4.2 メニュー項目

メニューバーアイコンをクリックすると以下のメニューを表示する:

```
┌─────────────────────────┐
│ ● Running               │  ← ステータス表示（Running / Stopped / Error）
│─────────────────────────│
│ Start                   │  ← 停止中のみ表示
│ Restart                 │  ← 起動中のみ表示
│ Stop                    │  ← 起動中のみ表示
│─────────────────────────│
│ Config...               │  ← 設定ファイルを開く
│ Log...                  │  ← ログファイルを開く
│─────────────────────────│
│ ☐ Launch at Login       │  ← ログイン時自動起動トグル
│─────────────────────────│
│ Quit                    │  ← アプリ終了
└─────────────────────────┘
```

## 5. 機能詳細

### 5.1 プロセス管理

Foundation `Process` でcoopバイナリを子プロセスとして起動・管理する。

**起動:**
```
coop --config ~/.config/coop/coop-local.dhall
```

- `stdout` / `stderr` をログファイルにリダイレクト
- プロセスの終了を `waitUntilExit` + `terminationHandler` で監視

**停止:**
- `SIGTERM` を送信
- 3秒以内に終了しなければ `SIGKILL`

**アプリ終了時:**
- coopプロセスが起動中であれば、上記の停止手順でクリーンアップしてからアプリを終了する

### 5.2 異常終了時の自動再起動

プロセスが異常終了（exit code != 0）した場合、exponential backoffで自動再起動を試みる。

| 試行 | 待機時間 |
|------|----------|
| 1回目 | 1秒 |
| 2回目 | 2秒 |
| 3回目 | 4秒 |
| 4回目 | 8秒 |
| 5回目 | 16秒 |

- 最大5回まで再起動を試行
- 5回すべて失敗した場合、状態を「エラー」に遷移しメニューバーアイコンを赤に変更
- ユーザーが手動でStart/Restartした場合、再起動カウンタをリセット

### 5.3 Config

`~/.config/coop/coop-local.dhall` をデフォルトエディタで開く。

- ファイルが存在しない場合:
  1. `~/.config/coop/` ディレクトリを作成（なければ）
  2. デフォルト内容でファイルを作成（`config/coop-local.dhall.example` の内容をベースとする）
  3. エディタで開く
- `NSWorkspace.shared.open()` でシステムデフォルトのエディタを使用

### 5.4 Log

coopプロセスの出力をファイルに書き出し、閲覧時にシステムのデフォルトテキストビューア（Console.app またはデフォルトエディタ）で開く。

- ログファイルパス: `~/.config/coop/coop.log`
- coopプロセスの `stdout` / `stderr` を `FileHandle` 経由でこのファイルに書き出す
- メニューの「Log...」クリックで `NSWorkspace.shared.open()` により開く

### 5.5 ログイン時自動起動

`SMAppService.mainApp` を使用してログイン時自動起動をトグルする。

- メニューの「Launch at Login」チェックボックスで有効/無効を切り替え
- 状態は `SMAppService.mainApp.status` から取得

## 6. 状態管理

アプリ全体の状態を `@Observable` クラスで管理する:

```swift
@Observable
class CoopManager {
    enum Status {
        case stopped
        case running
        case error(String)
    }

    var status: Status = .stopped
    var launchAtLogin: Bool  // SMAppService連動
}
```

## 7. ファイルパス一覧

| パス | 用途 |
|------|------|
| `~/.config/coop/coop-local.dhall` | coop設定ファイル |
| `~/.config/coop/coop.log` | coopプロセスのログ出力先 |
| `config/coop-local.dhall.example` | 設定ファイルのテンプレート（リポジトリ内） |

---

## Phase 2: 配布とバイナリ同梱

### 8. coopバイナリの同梱

Phase 1ではPATH上の `coop` を使用するが、Phase 2ではアプリバンドル内にcoopバイナリを同梱し、単体で動作可能にする。

#### 8.1 バンドル配置

```
CoopApp.app/
  Contents/
    Info.plist
    MacOS/
      CoopApp          ← Swift メインバイナリ
      coop             ← Haskell バイナリ
    Frameworks/        ← 動的ライブラリ（必要な場合）
    Resources/
```

- ヘルパーバイナリは `Contents/MacOS/` に配置（codesign対象として自動認識される）
- Swift側からの参照: `Bundle.main.bundleURL.appendingPathComponent("Contents/MacOS/coop")`

#### 8.2 動的ライブラリの処理

Haskellバイナリは `libgmp` 等の非システムdylibに依存する可能性がある。

**方針: GHC native bignum バックエンドを使い、libgmp依存を排除する**

```
-- cabal.project
package ghc-bignum
  flags: +native
```

GHC 9.2+ では native bignum バックエンドが利用可能。これにより `libgmp.dylib` への依存がなくなり、バンドルが大幅に簡素化される。

**依存確認:**
```bash
otool -L dist-newstyle/build/.../coop
```

`/usr/lib/` と `/System/` 以下のみであればバンドル不要。それ以外の依存がある場合は `dylibbundler` で `Contents/Frameworks/` に収集し、rpathを書き換える:

```bash
dylibbundler -od -b \
  -x CoopApp.app/Contents/MacOS/coop \
  -d CoopApp.app/Contents/Frameworks/ \
  -p @executable_path/../Frameworks/
```

#### 8.3 Xcodeでの組み込み

1. Build Phases → **Copy Files** phase追加、destination = Executables
2. ビルド済みcoopバイナリをコピー対象に指定
3. Run Script phaseでcoopバイナリをcodesign（アプリ署名の前に実行）

#### 8.4 ユニバーサルバイナリ（arm64 + x86_64）

GHCはアーキテクチャ別にビルドが必要。`lipo` で結合する:

```bash
# arm64 (Apple Silicon)
cabal build   # → coop-arm64

# x86_64 (Intel)
cabal build   # → coop-x86_64

# 結合
lipo -create -output coop coop-arm64 coop-x86_64
```

**CI戦略:** GitHub Actionsで `macos-latest`（arm64）と `macos-13`（x86_64）のmatrix buildを行い、最終ステップで `lipo` 結合。

**初期方針:** arm64のみで開始し、必要に応じてユニバーサル化する（Intel macは Rosetta 2で動作可能）。

### 9. 配布

#### 9.1 コード署名とNotarization

Apple Developer Program（$99/年）が必要。macOS 10.15以降、直接配布にはcodesign + notarizationが必須。

**署名手順:**
```bash
# 1. 内部バイナリを先に署名
codesign -s "Developer ID Application: ..." \
  --options runtime --timestamp \
  CoopApp.app/Contents/MacOS/coop

# 2. アプリ全体を署名
codesign -s "Developer ID Application: ..." \
  --options runtime --timestamp \
  CoopApp.app

# 3. Notarization
ditto -c -k --keepParent CoopApp.app CoopApp.zip
xcrun notarytool submit CoopApp.zip --keychain-profile "..." --wait

# 4. Staple
xcrun stapler staple CoopApp.app
```

- `--options runtime`（hardened runtime）はnotarizationの必須要件
- 内部の全Mach-Oを同一証明書で署名すること

#### 9.2 DMGによる配布

```bash
hdiutil create -volname "Coop" -srcfolder CoopApp.app \
  -ov -format UDZO CoopApp.dmg
```

- DMG自体もcodesign + notarize + staple する
- drag-to-Applicationsレイアウトにする

#### 9.3 配布チャネル

| チャネル | 用途 | 優先度 |
|----------|------|--------|
| GitHub Releases | DMGをアップロード。プライマリ配布先 | 必須 |
| Homebrew tap | `brew install --cask coop-app` で導入可能に | あると便利 |
| Sparkle | アプリ内自動アップデート | あると便利 |

#### 9.4 Homebrew tap

個人tapで即座に配布可能（公式caskはレビューが必要）:

```ruby
# homebrew-coop/Casks/coop-app.rb
cask "coop-app" do
  version "1.0.0"
  sha256 "..."
  url "https://github.com/.../releases/download/v#{version}/CoopApp-#{version}.dmg"
  name "Coop"
  desc "AI work assistant - macOS menu bar app"
  homepage "https://github.com/..."
  app "CoopApp.app"
end
```

`brew tap user/coop && brew install --cask coop-app`

#### 9.5 Sparkle（自動アップデート）

[Sparkle 2](https://sparkle-project.org/) をSPMで導入。EdDSA署名でアップデートを検証。

- メニューに「Check for Updates...」を追加
- GitHub Pages等にappcast.xmlをホスト
- リリースごとにDMGを署名してappcastを更新

### 10. Phase 2 実装順序

1. **coopバイナリの依存整理** — native bignum確認、`otool -L` で非システム依存がないことを確認
2. **Xcodeプロジェクトにバイナリ同梱** — Copy Files phase、バンドルからの起動パスに切り替え（PATHフォールバック付き）
3. **Apple Developer登録・署名設定** — 証明書取得、Xcodeの signing設定
4. **DMGビルド・Notarization** — スクリプト化
5. **GitHub Releasesで配布**
6. **Homebrew tap作成**（任意）
7. **Sparkle導入**（任意）
