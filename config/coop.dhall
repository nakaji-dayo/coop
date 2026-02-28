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
, cfgLLM =
  { llmBackend = env:LLM_BACKEND as Text ? "Claude"
  , llmClaude =
    { claudeApiKey = env:CLAUDE_API_KEY as Text ? ""
    , claudeModel = "claude-sonnet-4-20250514"
    }
  , llmOpenAI =
    { openaiApiKey = env:OPENAI_API_KEY as Text ? ""
    , openaiModel = env:OPENAI_MODEL as Text ? "gpt-4o"
    }
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
  , notionPropAssignee = env:NOTION_PROP_ASSIGNEE as Text ? ""
  , notionAssigneeUserId = env:NOTION_ASSIGNEE_USER_ID as Text ? ""
  , notionPropEstimate = env:NOTION_PROP_ESTIMATE as Text ? ""
  }
, cfgDryrun =
  { dryrunDataDir = "config/dryrun-data"
  }
, cfgScheduler =
  { schedulerBriefingCron = env:SCHEDULER_BRIEFING_CRON as Text ? "* * * * *"
  }
, cfgGoogleCalendar =
  { googleClientId = env:GOOGLE_CLIENT_ID as Text ? ""
  , googleClientSecret = env:GOOGLE_CLIENT_SECRET as Text ? ""
  , googleCalendarId = env:GOOGLE_CALENDAR_ID as Text ? "primary"
  , googleTokenPath = env:GOOGLE_TOKEN_PATH as Text ? "~/.config/coop/google-token.json"
  }
}
