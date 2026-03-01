# Slack App セットアップ

## 1. アプリ作成

[Slack API](https://api.slack.com/apps) で新しいアプリを作成する。

## 2. Socket Mode を有効化

- **Settings > Socket Mode** → 有効化
- App-Level Token を生成（スコープ: `connections:write`）
- 生成されたトークン (`xapp-...`) を `SLACK_APP_TOKEN` に設定

## 3. Event Subscriptions

- **Features > Event Subscriptions** → 有効化
- **Subscribe to bot events** に以下を追加:
  - `message.channels` — パブリックチャンネルのメッセージ受信
  - `message.groups` — プライベートチャンネルのメッセージ受信（必要な場合）

> Webhook モードの場合は Request URL に `https://<host>/slack/events` を設定する。

## 4. OAuth & Permissions

**Bot Token Scopes** に以下を追加:

| スコープ | 用途 |
|---|---|
| `chat:write` | タスク作成結果のスレッド返信・通知チャンネルへの投稿 |
| `channels:history` | パブリックチャンネルのメッセージ受信 |
| `groups:history` | プライベートチャンネルのメッセージ受信（必要な場合） |

スコープ追加後、**Install to Workspace** でワークスペースに再インストールする。

## 5. トークン・IDの取得

| 値 | 取得場所 |
|---|---|
| `SLACK_BOT_TOKEN` (`xoxb-...`) | **OAuth & Permissions** > Bot User OAuth Token |
| `SLACK_SIGNING_SECRET` | **Basic Information** > Signing Secret |
| `SLACK_APP_TOKEN` (`xapp-...`) | **Basic Information** > App-Level Tokens |
| `SLACK_BOT_USER_ID` (`U...`) | Slack上でbotのプロフィールを開き、IDをコピー |
| `SLACK_MONITORED_USER_ID` (`U...`) | 監視対象ユーザーのプロフィールからIDをコピー |
| `SLACK_NOTIFY_CHANNEL` (`C...`) | 通知先チャンネルのチャンネルIDをコピー |

## 6. チャンネルへの招待

botを以下のチャンネルに招待する（チャンネル内で `/invite @bot名`）:

- メンションを監視するチャンネル
- 通知先チャンネル（`SLACK_NOTIFY_CHANNEL`）
