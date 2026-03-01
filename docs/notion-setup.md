# Notion セットアップ

## 1. インテグレーション作成

[Notion Integrations](https://www.notion.so/my-integrations) で新しいインテグレーションを作成する。

- 種類: Internal
- 権限: コンテンツの読み取り・更新・挿入

生成されたトークン (`ntn_...`) を `NOTION_API_KEY` に設定する。

## 2. タスク管理用データベースの準備

Notion上でタスク管理用のデータベースを作成する。以下のプロパティを設定:

| プロパティ | タイプ | 環境変数 | デフォルト |
|---|---|---|---|
| タスク名 | Title | `NOTION_PROP_NAME` | `Name` |
| 優先度 | Select | `NOTION_PROP_PRIORITY` | `Priority` |
| ステータス | Status | `NOTION_PROP_STATUS` | `Status` |
| 期日 | Date | `NOTION_PROP_DUE_DATE` | （空 = 使わない） |

**Priority (Select)** のオプション: `Critical`, `High`, `Medium`, `Low`

**Status** のオプション名はデフォルトで `Open`, `In Progress`, `Done` を想定。
カスタム名を使う場合は環境変数で指定:

```
NOTION_STATUS_OPEN=未着手
NOTION_STATUS_IN_PROGRESS=進行中
NOTION_STATUS_DONE=完了
```

> Priority や Status を使わない場合は、対応する環境変数を空文字に設定するとスキップされる。

## 3. データベースとページの共有

作成したインテグレーションに以下を共有する（各ページ右上の「...」→「コネクト」→インテグレーション名を選択）:

- タスク管理データベース → `NOTION_TASK_DATABASE_ID`
- 行動指針ページ → `NOTION_GUIDELINES_PAGE_ID`
- LLM指示ページ → `NOTION_INSTRUCTIONS_PAGE_ID`

## 4. ページIDの取得

NotionのページURLからIDを取得する:

```
https://www.notion.so/ページ名-<32文字のID>
                               ^^^^^^^^^^^^^^^^
```

ハイフンを含む場合はそのまま使用可能。
