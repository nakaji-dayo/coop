{ cfgMode = < Dryrun | Live >.Dryrun
, cfgPort = 3000
, cfgLogLevel = "INFO"
, cfgSlack =
  { slackBotToken = "xoxb-dryrun-token"
  , slackSigningSecret = "dryrun-signing-secret"
  , slackBotUserId = "U_DRYRUN_BOT"
  , slackMonitoredUserId = "U_DRYRUN_MONITORED"
  , slackNotifyChannel = "C_DRYRUN_NOTIFY"
  , slackAppToken = ""
  , slackConnectionMode = < Webhook | SocketMode >.Webhook
  }
, cfgClaude =
  { claudeApiKey = "dryrun-claude-key"
  , claudeModel = "claude-sonnet-4-20250514"
  }
, cfgNotion =
  { notionApiKey = "dryrun-notion-key"
  , notionTaskDatabaseId = "dryrun-task-db"
  , notionGuidelinesPageId = "guidelines"
  , notionInstructionsPageId = "llm-instructions"
  , notionPropName = "Name"
  , notionPropPriority = "Priority"
  , notionPropStatus = "Status"
  , notionPropDueDate = ""
  , notionStatusOpen = "Open"
  , notionStatusInProgress = "In Progress"
  , notionStatusDone = "Done"
  }
, cfgDryrun =
  { dryrunDataDir = "config/dryrun-data"
  }
}
