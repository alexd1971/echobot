module Bot where

import DSL.BotLang

botScript :: BotScript ()
botScript = do
  updates <- getUpdates
  mapM_ processUpdate updates

processUpdate :: Update -> BotScript ()
processUpdate update = do
  case update of
    Message _ -> echoMessage update
    Command _ -> executeCommand update
    CallBackData _ -> setUserPreferences update
