let Mode = < Dryrun | Live >
let ConnMode = < Webhook | SocketMode >
in
{ cfgMode = if env:COOP_DRYRUN ? False then Mode.Dryrun else Mode.Live
, cfgPort = 3000
, cfgLogLevel = env:LOG_LEVEL as Text ? "INFO"
, cfgSlack =
  { slackBotToken = env:SLACK_BOT_TOKEN as Text
  , slackSigningSecret = env:SLACK_SIGNING_SECRET as Text
  , slackBotUserId = env:SLACK_BOT_USER_ID as Text
  , slackMonitoredUserId = env:SLACK_MONITORED_USER_ID as Text
  , slackNotifyChannel = env:SLACK_NOTIFY_CHANNEL as Text
  , slackAppToken = env:SLACK_APP_TOKEN as Text ? ""
  , slackConnectionMode = if env:SLACK_SOCKET_MODE ? False then ConnMode.SocketMode else ConnMode.Webhook
  }
, cfgClaude =
  { claudeApiKey = env:CLAUDE_API_KEY as Text
  , claudeModel = "claude-sonnet-4-20250514"
  }
, cfgNotion =
  { notionApiKey = env:NOTION_API_KEY as Text
  , notionTaskDatabaseId = env:NOTION_TASK_DATABASE_ID as Text
  , notionGuidelinesPageId = env:NOTION_GUIDELINES_PAGE_ID as Text
  , notionInstructionsPageId = env:NOTION_INSTRUCTIONS_PAGE_ID as Text
  , notionPropName = env:NOTION_PROP_NAME as Text ? "Name"
  , notionPropPriority = env:NOTION_PROP_PRIORITY as Text ? "Priority"
  , notionPropStatus = env:NOTION_PROP_STATUS as Text ? "Status"
  , notionPropDueDate = env:NOTION_PROP_DUE_DATE as Text ? ""
  , notionStatusOpen = env:NOTION_STATUS_OPEN as Text ? "Open"
  , notionStatusInProgress = env:NOTION_STATUS_IN_PROGRESS as Text ? "In Progress"
  , notionStatusDone = env:NOTION_STATUS_DONE as Text ? "Done"
  }
, cfgDryrun =
  { dryrunDataDir = "config/dryrun-data"
  }
}
