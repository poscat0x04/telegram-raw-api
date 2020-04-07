{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Web.Telegram.API.Lens
  ( module Web.Telegram.Types.Lens,
    disableWebPagePreview,
    parseMode,
    disableNotification,
    replyToMessageId,
    fromChatId,
    action,
    inlineMessageId,
    inlineQueryId,
    results,
    cacheTime,
    isPersonal,
    nextOffset,
    switchPmText,
    switchPmParameter,
    offset,
    limit,
    timeout,
    allowedUpdates,
    url,
    maxConnections,
  )
where

import Control.Lens
import Data.Generics.Product
import Web.Telegram.Types.Lens

disableWebPagePreview :: HasField "disableWebPagePreview" s t a b => Lens s t a b
disableWebPagePreview = field @"disableWebPagePreview"

parseMode :: HasField "parseMode" s t a b => Lens s t a b
parseMode = field @"parseMode"

disableNotification :: HasField "disableNotification" s t a b => Lens s t a b
disableNotification = field @"disableNotification"

replyToMessageId :: HasField "replyToMessageId" s t a b => Lens s t a b
replyToMessageId = field @"replyToMessageId"

fromChatId :: HasField "fromChatId" s t a b => Lens s t a b
fromChatId = field @"fromChatId"

action :: HasField "action" s t a b => Lens s t a b
action = field @"action"

inlineMessageId :: HasField "inlineMessageId" s t a b => Lens s t a b
inlineMessageId = field @"inlineMessageId"

inlineQueryId :: HasField "inlineQueryId" s t a b => Lens s t a b
inlineQueryId = field @"inlineQueryId"

results :: HasField "results" s t a b => Lens s t a b
results = field @"results"

cacheTime :: HasField "cacheTime" s t a b => Lens s t a b
cacheTime = field @"cacheTime"

isPersonal :: HasField "isPersonal" s t a b => Lens s t a b
isPersonal = field @"isPersonal"

nextOffset :: HasField "nextOffset" s t a b => Lens s t a b
nextOffset = field @"nextOffset"

switchPmText :: HasField "switchPmText" s t a b => Lens s t a b
switchPmText = field @"switchPmText"

switchPmParameter :: HasField "switchPmParameter" s t a b => Lens s t a b
switchPmParameter = field @"switchPmParameter"

offset :: HasField "offset" s t a b => Lens s t a b
offset = field @"offset"

limit :: HasField "limit" s t a b => Lens s t a b
limit = field @"limit"

timeout :: HasField "timeout" s t a b => Lens s t a b
timeout = field @"timeout"

allowedUpdates :: HasField "allowedUpdates" s t a b => Lens s t a b
allowedUpdates = field @"allowedUpdates"

url :: HasField "url" s t a b => Lens s t a b
url = field @"url"

maxConnections :: HasField "maxConnections" s t a b => Lens s t a b
maxConnections = field @"maxConnections"
