let Mode = < Dryrun | Live >
let ConnMode = < Webhook | SocketMode >
in
{ cfgMode = Mode.Live
, cfgPort = 3000
, cfgLogLevel = "INFO"
, cfgSlack =
  { slackBotToken = ""
  , slackSigningSecret = ""
  , slackBotUserId = ""
  , slackMonitoredUserId = ""
  , slackNotifyChannel = ""
  , slackAppToken = ""
  , slackConnectionMode = ConnMode.Webhook
  , slackCatchupChannels = ""
  }
, cfgLLM =
  { llmBackend = "Claude"
  , llmClaude =
    { claudeApiKey = ""
    , claudeModel = "claude-sonnet-4-20250514"
    }
  , llmOpenAI =
    { openaiApiKey = ""
    , openaiModel = "gpt-4o"
    }
  }
, cfgNotion =
  { notionApiKey = ""
  , notionTaskDatabaseId = ""
  , notionGuidelinesPageId = ""
  , notionInstructionsPageId = ""
  , notionPropName = "Name"
  , notionPropPriority = "Priority"
  , notionPropStatus = "Status"
  , notionPropDueDate = ""
  , notionStatusOpen = "Open"
  , notionStatusInProgress = "In Progress"
  , notionStatusDone = [ "Done" ]
  , notionPropAssignee = ""
  , notionAssigneeUserId = ""
  , notionPropEstimate = ""
  }
, cfgDryrun =
  { dryrunDataDir = "config/dryrun-data"
  }
, cfgScheduler =
  { schedulerBriefingCron = "* * * * *"
  , schedulerWeeklyBriefingCron = "-"
  , schedulerWeeklyAvailableHours = 30
  , schedulerEstimateUnit = < Minutes | Hours | Days | Points >.Hours
  }
, cfgGoogleCalendar =
  { googleClientId = ""
  , googleClientSecret = ""
  , googleCalendarId = "primary"
  , googleTokenPath = "~/.config/coop/google-token.json"
  }
}
