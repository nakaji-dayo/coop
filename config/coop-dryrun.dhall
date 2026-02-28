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
, cfgLLM =
  { llmBackend = "Claude"
  , llmClaude =
    { claudeApiKey = "dryrun-claude-key"
    , claudeModel = "claude-sonnet-4-20250514"
    }
  , llmOpenAI =
    { openaiApiKey = "dryrun-openai-key"
    , openaiModel = "gpt-4o"
    }
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
  , notionPropAssignee = ""
  , notionAssigneeUserId = ""
  , notionPropEstimate = ""
  }
, cfgDryrun =
  { dryrunDataDir = "config/dryrun-data"
  }
, cfgScheduler =
  { schedulerBriefingCron = "0 9 * * *"
  }
, cfgGoogleCalendar =
  { googleClientId = ""
  , googleClientSecret = ""
  , googleCalendarId = ""
  , googleTokenPath = ""
  }
}
