# Google Calendar セットアップ

## 1. OAuth 2.0 クライアントの作成

[Google Cloud Console](https://console.cloud.google.com/apis/credentials) でOAuth 2.0クライアントを作成する。

- アプリケーションの種類: デスクトップアプリ
- 作成後、Client ID と Client Secret を取得

## 2. Google Calendar API の有効化

[Google Calendar API](https://console.cloud.google.com/apis/library/calendar-json.googleapis.com) を有効化する。

## 3. 環境変数の設定

| 変数名 | 説明 |
|---|---|
| `GOOGLE_CLIENT_ID` | OAuth 2.0 Client ID |
| `GOOGLE_CLIENT_SECRET` | OAuth 2.0 Client Secret |
| `GOOGLE_REFRESH_TOKEN` | リフレッシュトークン（認証後に取得） |
| `GOOGLE_TOKEN_PATH` | トークン保存パス（デフォルト: `~/.config/coop/google-token.json`） |

## 4. 認証

以下のコマンドでブラウザ認証を行い、refresh token を取得する:

```sh
cabal run coop -- auth google
```

ブラウザが開き、Googleアカウントでの認証・権限付与を求められる。
認証完了後、トークンが `GOOGLE_TOKEN_PATH` に保存される。

## 5. 利用されるスコープ

- `calendar.events.readonly` — カレンダー予定の読み取りのみ

## 6. 取得される情報

- イベントタイトル
- 開始・終了時刻
- 参加予定（参加・欠席・未定）

これらの情報はデイリーブリーフィングでMTGを考慮したスケジュール提案に使用される。
