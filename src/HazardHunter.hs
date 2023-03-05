{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
module HazardHunter (run, mineSweeperApp) where

import Butler
import Butler.App (Display (..))
import Butler.Display.Session (Session (..), UserName, changeUsername)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time (diffUTCTime)
import HazardHunter.Engine
import HazardHunter.Htmx (mkHxVals)
import Text.Printf (printf)
import Prelude

run :: IO ()
run = void $ runMain $ spawnInitProcess ".butler-storage" $ standaloneGuiApp

standaloneGuiApp :: ProcessIO Void
standaloneGuiApp = serveApps publicDisplayApp [mineSweeperApp]

mineSweeperApp :: App
mineSweeperApp =
  (defaultApp "hazard-hunter" startMineSweeper)
    { tags = fromList ["Game"],
      description = "game"
    }

handleEvent :: UserName -> DisplayClients -> WinID -> MSEvent -> TVar MSState -> ProcessIO ()
handleEvent username clients wId msEv appStateV = do
  case msEv of
    NewGame -> do
      atomically $ modifyTVar' appStateV $ \s -> s {state = Wait}
      sendsHtml clients $ renderPanel wId appStateV (Just 0.0)
      sendsHtml clients $ renderSettings username wId appStateV
    SettingsSelected level boardColor -> do
      newBoard <- liftIO . initBoard $ levelToBoardSettings level
      hazard <- liftIO randomHazard
      atomically $ do
        modifyTVar' appStateV $ \s ->
          s
            { board = newBoard,
              state = Wait,
              settings = MSSettings level boardColor hazard
            }
      sendsHtml clients $ renderApp wId appStateV
    SetFlagMode -> do
      join $ atomically $ do
        appState <- readTVar appStateV
        case appState.state of
          Play st fm -> do
            modifyTVar' appStateV $ \s -> s {state = Play st (not fm)}
            pure $ sendsHtml clients $ renderFlag wId appStateV
          _ -> pure $ pure ()
    ClickCell cellCoord -> do
      atTime <- liftIO getCurrentTime
      appState' <- readTVarIO appStateV
      case countOpenCells appState'.board of
        0 -> do
          newBoard <- liftIO $ ensureNFBoard appState'.board cellCoord appState'.settings.level
          atomically $ modifyTVar' appStateV $ \s -> s {board = newBoard}
          pure ()
        _ -> pure ()
      case appState'.state of
        Wait -> do
          atomically $ do
            modifyTVar' appStateV $ \s -> s {state = Play atTime False}
        _ -> pure ()
      appState <- readTVarIO appStateV
      case appState.state of
        Play _ False -> do
          let playDuration = mkPlayDuration appState.state atTime
          case isFlagCell cellCoord appState.board of
            True -> pure ()
            False -> do
              case isMineCell cellCoord appState.board of
                True -> do
                  atomically $ do
                    modifyTVar' appStateV $ \s ->
                      s
                        { board = openCell cellCoord appState.board,
                          state = Gameover
                        }
                  sendsHtml clients $ renderBoard wId appStateV
                  sendsHtml clients $ renderPanel wId appStateV (Just playDuration)
                False -> do
                  let gs1 = openCell cellCoord appState.board
                      gs2 = openAdjBlank0Cells (levelToBoardSettings appState.settings.level) cellCoord gs1
                  case countHiddenBlank gs2 == 0 of
                    True -> do
                      atomically $ do
                        modifyTVar' appStateV $ \s -> s {board = gs2, state = Win}
                      sendsHtml clients $ renderBoard wId appStateV
                      sendsHtml clients $ renderPanel wId appStateV (Just playDuration)
                    -- addScore dbConn appState.settings.playerName atTime playDuration appState.settings.level
                    -- leaderBoard <- renderLeaderBoard appStateV dbConn
                    -- pure [board, panel, leaderBoard]
                    False -> do
                      atomically $ do
                        modifyTVar' appStateV $ \s -> s {board = gs2}
                      sendsHtml clients $ renderBoard wId appStateV
                      sendsHtml clients $ renderSmiley wId appStateV
        Play _ True -> do
          atomically $ do
            let board = setFlagOnCell cellCoord appState.board
            modifyTVar' appStateV $ \s -> s {board}
          sendsHtml clients $ renderFlag wId appStateV
          sendsHtml clients $ renderBoard wId appStateV
        _ -> pure ()
  where
    ensureNFBoard :: MSBoard -> MSCellCoord -> MSLevel -> IO MSBoard
    ensureNFBoard board cellCoord level = case isMineCell cellCoord board of
      True -> do
        newBoard <- initBoard $ levelToBoardSettings level
        ensureNFBoard newBoard cellCoord level
      False -> pure board

mkPlayDuration :: MSGameState -> UTCTime -> Float
mkPlayDuration s curD = case s of
  Play startDate _ -> diffTimeToFloat curD startDate
  _ -> error "Should not happen"

diffTimeToFloat :: UTCTime -> UTCTime -> Float
diffTimeToFloat a b = realToFrac $ diffUTCTime a b

startMineSweeper :: AppContext -> ProcessIO ()
startMineSweeper ctx = do
  let level = defaultLevel
  board <- liftIO $ initBoard $ levelToBoardSettings level
  hazard <- liftIO randomHazard
  state <- newTVarIO $ MSState board Wait (MSSettings level Blue hazard)
  spawnThread_ $ asyncTimerUpdateThread state ctx.clients
  forever $ do
    res <- atomically (readPipe ctx.pipe)
    case res of
      AppDisplay _ -> sendHtmlOnConnect (renderApp ctx.wid state) res
      AppTrigger ev -> do
        mAppEvent <- toAppEvents ev.client ev.trigger ev.body
        username <- readTVarIO (ev.client.session.username)
        case mAppEvent of
          Just appEvent -> handleEvent username ctx.clients ctx.wid appEvent state
          _ -> pure ()
      _ -> pure ()
  where
    toAppEvents :: DisplayClient -> TriggerName -> Value -> ProcessIO (Maybe MSEvent)
    toAppEvents client tn td = case tn of
      TriggerName "play" -> do
        logInfo "Got <play> game event" []
        pure $ Just NewGame
      TriggerName "setFlagMode" -> do
        logInfo "Got <setFlagMode> game event" []
        pure $ Just SetFlagMode
      TriggerName "clickCell" -> do
        logInfo "Got <clickCell> game event" ["data" .= td]
        case (td ^? key "cx" . _String, td ^? key "cy" . _String) of
          (Just cxS, Just cyS) -> do
            let cxM = readMaybe $ from cxS :: Maybe Int
                cyM = readMaybe $ from cyS :: Maybe Int
            case (cxM, cyM) of
              (Just cx, Just cy) -> pure $ Just $ ClickCell (MSCellCoord cx cy)
              _ -> pure Nothing
          _ -> pure Nothing
      TriggerName "setSettings" -> do
        logInfo "Got <setSettings> game event" ["data" .= td]
        case ( td ^? key "level" . _String,
               td ^? key "playerName" . _JSON,
               td ^? key "boardColor" . _String
             ) of
          (Just level, Just playerName, Just boardColor) -> do
            clientName <- readTVarIO (client.session.username)
            when (playerName /= clientName) $ do
              unlessM (changeUsername ctx.shared.display.sessions client.session playerName) $
                logError "Username already taken" ["name" .= playerName]
            pure . Just $ SettingsSelected (from level) (from boardColor)
          _ -> pure Nothing
      _ -> do
        logInfo "Got unknown game event" ["TriggerName" .= tn]
        pure Nothing
    asyncTimerUpdateThread :: TVar MSState -> DisplayClients -> ProcessIO Void
    asyncTimerUpdateThread appStateV clients = forever $ do
      appState <- readTVarIO appStateV
      case appState.state of
        Play {} -> do
          atTime <- liftIO getCurrentTime
          let playDuration = mkPlayDuration appState.state atTime
          sendsHtml clients $ renderTimer playDuration
        _ -> pure ()
      sleep 990

withEvent :: Monad m => WinID -> Text -> [Attribute] -> HtmlT m () -> HtmlT m ()
withEvent wid tId tAttrs elm = with elm ([id_ (withWID wid tId), wsSend' ""] <> tAttrs)
  where
    wsSend' = makeAttribute "ws-send"

renderApp :: WinID -> TVar MSState -> HtmlT STM ()
renderApp wid state = do
  div_ [id_ (withWID wid "w"), class_ "border-2 border-gray-400 bg-gray-100"] $ do
    renderPanel wid state (Just 0.0)
    renderBoard wid state

withThemeBgColor :: Color -> Text -> Text -> Text
withThemeBgColor = withThemeColor "bg"

withThemeBorderColor :: Color -> Text -> Text -> Text
withThemeBorderColor = withThemeColor "border"

withThemeColor :: Text -> Color -> Text -> Text -> Text
withThemeColor cat color level cur = cur <> " " <> bgColorBase <> "-" <> level
  where
    colorBase = case color of
      Blue -> "blue"
      Pink -> "pink"
      Green -> "green"
    bgColorBase = cat <> "-" <> colorBase

toDurationT :: Float -> String
toDurationT = printf "%.1f"

renderTimer :: Float -> HtmlT STM ()
renderTimer duration = do
  div_ [id_ "MSTimer", class_ "w-24 text-right pr-1"] $ toHtml $ toDurationT duration

renderFlag :: WinID -> TVar MSState -> HtmlT STM ()
renderFlag wid appStateV = do
  appState <- lift $ readTVar appStateV
  let flagMode = case appState.state of
        Play _ True ->
          withThemeBgColor appState.settings.color "300" ""
        _ -> mempty
      usedFlags = countFlagCells appState.board
  div_ [id_ "MSFlag"] $ do
    div_ [class_ "flex flex-row gap-1"] $ do
      withEvent wid "setFlagMode" [] $
        div_ [class_ $ withThemeBorderColor appState.settings.color "300" "cursor-pointer border-2 rounded" <> flagMode] "🚩"
      div_ . toHtml $ "(" <> show usedFlags <> ")"

renderSmiley :: WinID -> TVar MSState -> HtmlT STM ()
renderSmiley wid appStateV = do
  appState <- lift $ readTVar appStateV
  div_ [id_ "MSSmiley"] $
    withEvent wid "play" [] $
      div_ [class_ $ withThemeBorderColor appState.settings.color "300" "px-1 cursor-pointer border-2 rounded whitespace-nowrap"] $
        ( case appState.state of
            Play _ _ -> "🤔"
            Wait -> "💤"
            Gameover -> "😖"
            Win -> "🥳"
        )
          <> " New Game"

renderPanel :: WinID -> TVar MSState -> Maybe Float -> HtmlT STM ()
renderPanel wid appStateV durationM = do
  let smiley = renderSmiley wid appStateV
      flag = renderFlag wid appStateV
  appState <- lift $ readTVar appStateV
  div_ [id_ "MSPanel", class_ $ withThemeBgColor appState.settings.color "200" "flex justify-between"] $ do
    let mineCount' = mineCount $ levelToBoardSettings appState.settings.level
    div_ [class_ "pl-1 w-24"] $ toHtml $ hazardLabel mineCount' appState.settings.hazard
    div_ [class_ "flex flex-row gap-2"] flag
    forM_ durationM renderTimer
    smiley
  where
    hazardLabel :: Int -> Hazard -> Text
    hazardLabel count hazard = from (show count) <> " " <> hazardToText hazard

renderBoard :: WinID -> TVar MSState -> HtmlT STM ()
renderBoard wid appStateV = do
  appState <- lift $ readTVar appStateV
  let sizeCount' = sizeCount $ levelToBoardSettings appState.settings.level
  let gridType = "grid-cols-[" <> T.intercalate "_" (Prelude.replicate (sizeCount' + 1) "20px") <> "]"
  div_ [id_ "MSBoard"] $ do
    div_ [class_ "flex place-content-center m-1"] $ do
      div_ [class_ $ "grid gap-1 " <> gridType] $ do
        mapM_ (renderCell appState.state appState.settings.hazard appState.settings.color) $
          Map.toList appState.board
  where
    renderCell :: MSGameState -> Hazard -> Color -> (MSCellCoord, MSCell) -> HtmlT STM ()
    renderCell gameState hazard color (cellCoords, cellState) =
      let cellId = mkHxVals [("cx", T.pack $ show $ cellCoords.cx), ("cy", T.pack $ show $ cellCoords.cy)]
       in installCellEvent gameState cellId $
            div_ [class_ $ withThemeBgColor color "100" "text-center cursor-pointer"] $
              case cellState of
                MSCell (Blank v) Open
                  | v == 0 -> div_ [class_ "h-6 w-full "] ""
                  | v == 1 -> div_ [class_ "font-bold text-blue-700"] $ showCellValue v
                  | v == 2 -> div_ [class_ "font-bold text-green-700"] $ showCellValue v
                  | v == 3 -> div_ [class_ "font-bold text-red-700"] $ showCellValue v
                  | v == 4 -> div_ [class_ "font-bold text-blue-900"] $ showCellValue v
                  | v == 5 -> div_ [class_ "font-bold text-red-900"] $ showCellValue v
                  | v == 6 -> div_ [class_ "font-bold text-green-900"] $ showCellValue v
                  | v == 7 -> div_ [class_ "font-bold text-brown-700"] $ showCellValue v
                  | v == 8 -> div_ [class_ "font-bold text-black-700"] $ showCellValue v
                MSCell (Blank _) Open -> error "Impossible case"
                MSCell Mine Open -> mineCell
                MSCell (Blank _) (Hidden True) -> flagCell
                MSCell Mine (Hidden flag) -> case gameState of
                  Gameover -> mineCell
                  _ -> if flag then flagCell else hiddenCell
                MSCell _ _ -> hiddenCell
      where
        mineCell = div_ [class_ "bg-red-500"] $ toHtml $ hazardToText hazard
        hiddenCell = div_ [class_ $ withThemeBgColor color "300" "border-2 rounded border-r-gray-400 border-b-gray-400 h-6 w-full"] ""
        flagCell = div_ [class_ $ withThemeBgColor color "300" "border-2 rounded border-r-gray-400 border-b-gray-400 h-6 w-full"] "🚩"
        showCellValue :: Int -> HtmlT STM ()
        showCellValue = toHtml . show
        installCellEvent :: MSGameState -> Attribute -> HtmlT STM () -> HtmlT STM ()
        installCellEvent gs cellId elm =
          let elm' = withEvent wid "clickCell" [cellId] elm
           in case gs of
                Play _ _ -> elm'
                Wait -> elm'
                _ -> elm

renderSettings :: UserName -> WinID -> TVar MSState -> HtmlT STM ()
renderSettings username wid appStateV = do
  appState <- lift $ readTVar appStateV
  let selectedLevel = appState.settings.level
  div_ [id_ "MSBoard"] $ do
    withEvent wid "setSettings" [] $ do
      form_ [class_ $ withThemeBgColor appState.settings.color "100" "flex flex-col items-center gap-px"] $ do
        div_ [class_ "my-2"] $ startButton appState
        label_ [class_ "m-1 font-semibold"] "Set the board color"
        colorInput appState.settings.color
        label_ [class_ "m-1 font-semibold"] "Set your name"
        nameInput username
        label_ [class_ "m-1 font-semibold"] "Select a level"
        mapM_ (div_ . levelButton selectedLevel) [minBound .. maxBound]
  where
    colorInput :: Color -> HtmlT STM ()
    colorInput colorFromSettings =
      select_
        [ class_ "block mb-2 text-center border border-slate-300 rounded-md",
          name_ "boardColor"
        ]
        $ mapM_ (\c -> option_ (setValue c) (toHtml $ show c)) [minBound .. maxBound]
      where
        setValue :: Color -> [Attribute]
        setValue color =
          let selected = if colorFromSettings == color then [selected_ ""] else mempty
           in [value_ . from $ show color] <> selected
    nameInput :: UserName -> HtmlT STM ()
    nameInput playerName =
      input_
        [ type_ "text",
          name_ "playerName",
          value_ (into @Text playerName),
          placeholder_ "Anonymous",
          size_ "15",
          maxlength_ "15",
          class_ "h-8 text-center border border-slate-300 rounded-md focus:border-slate-400"
        ]
    startButton :: MSState -> HtmlT STM ()
    startButton appState =
      button_
        [ type_ "submit",
          class_ $ withThemeBorderColor appState.settings.color "300" "p-1 border-4 rounded"
        ]
        $ div_ [class_ "px-6 font-bold"] "Play"
    levelButton :: MSLevel -> MSLevel -> HtmlT STM ()
    levelButton selectedLevel level = do
      input_ $
        [name_ "level", id_ levelT, type_ "radio", value_ levelT]
          <> if level == selectedLevel then [checked_] else mempty
      label_ [for_ levelT] $ span_ [class_ levelS] $ toHtml levelT
      where
        levelT :: Text
        levelT = from $ show level
        levelS =
          "ml-2" <> " " <> case level of
            Baby -> "text-blue-700"
            Beginner -> "text-blue-800"
            Intermediate -> "text-green-700"
            Expert -> "text-red-700"
            Specialist -> "text-red-900"
            Survivalist -> "text-violet-900"
