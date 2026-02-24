# coop

AIをバックエンドとした、タスク管理・Slackメンション対応を支援するツール。
Slackでメンションされると行動指針を踏まえた優先度判定付きの通知が通知用チャンネルに届き、Notionにタスクが作られる。

## 必要な環境

- GHC 9.6
- cabal 3.10+

## ビルド

```sh
cabal build
```

## テスト

```sh
cabal test
```

## 起動

### Dryrunモード

外部サービス（Slack / Claude / Notion）への実際のリクエストを行わず、すべてログ出力に置き換えるモード。API keyは不要。

```sh
cabal run coop -- --config config/coop-dryrun.dhall
```

起動後の動作確認:

```sh
# ヘルスチェック
curl http://localhost:3000/health

# Slack URL verification（Slack App設定時に必要）
curl -X POST http://localhost:3000/slack/events \
  -H "Content-Type: application/octet-stream" \
  -d '{"type":"url_verification","challenge":"test"}'

# メンションイベントのシミュレーション
curl -X POST http://localhost:3000/slack/events \
  -H "Content-Type: application/octet-stream" \
  -d '{"type":"event_callback","event":{"type":"message","channel":"C001","ts":"1234.5678","user":"U999","text":"<@U_DRYRUN_BOT> 本番のログインバグを直して"}}'
```

### Liveモード

環境変数を設定してからLive用の設定ファイルで起動する。

```sh
export SLACK_BOT_TOKEN="xoxb-..."
export SLACK_SIGNING_SECRET="..."
export SLACK_BOT_USER_ID="U..."
export SLACK_NOTIFY_CHANNEL="C..."
export CLAUDE_API_KEY="sk-ant-..."
export NOTION_API_KEY="ntn_..."
export NOTION_TASK_DATABASE_ID="..."
export NOTION_GUIDELINES_PAGE_ID="..."
export NOTION_INSTRUCTIONS_PAGE_ID="..."

cabal run coop -- --config config/coop.dhall
```

## 環境変数一覧

| 変数名 | 説明 |
|--------|------|
| `SLACK_BOT_TOKEN` | Slack Bot User OAuth Token (`xoxb-...`) |
| `SLACK_SIGNING_SECRET` | Slack App の Signing Secret |
| `SLACK_BOT_USER_ID` | Bot の User ID（`U...`） |
| `SLACK_NOTIFY_CHANNEL` | 通知先チャンネルID（`C...`） |
| `CLAUDE_API_KEY` | Anthropic API key |
| `NOTION_API_KEY` | Notion Integration Token |
| `NOTION_TASK_DATABASE_ID` | タスク管理用 Notion Database の ID |
| `NOTION_GUIDELINES_PAGE_ID` | 行動指針ドキュメントの Notion Page ID |
| `NOTION_INSTRUCTIONS_PAGE_ID` | LLM指示ドキュメントの Notion Page ID |

## Docker

```sh
# Dryrunモードで起動
docker compose up

# Liveモードで起動（.envファイルまたは環境変数を設定済みの前提）
docker compose run -e CONFIG=config/coop.dhall coop --config config/coop.dhall
```

ngrok の管理画面: http://localhost:4040

## 設定ファイル

Dhall形式。`config/` 以下にテンプレートがある。

- `config/coop-dryrun.dhall` — Dryrunモード用（API key不要）
- `config/coop.dhall` — Liveモード用（環境変数から読み込み）

## プロジェクト構成

```
src/Coop/
  Config.hs              -- Dhall設定の型定義・読み込み
  Domain/                -- ドメイン型（Task, Mention, LLM, Doc, Notification）
  Effect/                -- 型クラスインターフェース（TaskStore, DocStore, LLM, Notifier）
  Adapter/
    Dryrun/              -- ログ出力・インメモリの疑似実装
    Claude/              -- Claude Messages API クライアント
    Notion/              -- Notion API クライアント（DocStore + TaskStore）
    Slack/               -- Slack chat.postMessage による通知
  Agent/
    Core.hs              -- メンション→分析→タスク作成→通知のパイプライン
    Context.hs           -- 行動指針・LLM指示の取得
    Prompt.hs            -- LLMプロンプト構築・レスポンスパース
  Server/
    API.hs               -- Servant API型定義
    Handlers.hs          -- リクエストハンドラ・署名検証
  App/
    Env.hs               -- Env レコード・Ops レコード
    Monad.hs             -- AppM モナド・型クラスインスタンス
    Log.hs               -- Katip ログ設定
```
