{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
module HazardHunter where

-- import Butler hiding (HtmxEvent (body))
import Butler
import Butler.Auth.Guest (guestAuthApp)
import Butler.Prelude
import qualified ButlerDemos as BD
-- import qualified Data.Aeson as Aeson
-- import qualified Data.Aeson.KeyMap as Aeson
-- import qualified Data.Aeson.Text as Aeson
import qualified Data.Map as Map
import qualified Data.Text as T
-- import Data.Text.Lazy (toStrict)

-- import System.Random (randomRIO)

import Data.Time (diffUTCTime, getCurrentTime)
import HazardHunter.Engine
import HazardHunter.Htmx (mkHxVals)
import Lucid.XStatic
import Text.Printf (printf)
import Prelude

run :: IO ()
run = BD.run standaloneGuiApp

standaloneGuiApp :: ProcessIO ()
standaloneGuiApp = BD.serveAppPerClient BD.defaultXFiles auth mineSweeperApp
  where
    auth = const . pure . guestAuthApp $ htmlMain BD.defaultXFiles "Standalone GUI" Nothing

htmlMain :: [XStaticFile] -> Text -> Maybe (Html ()) -> Html ()
htmlMain xfiles title mHtml = do
  doctypehtml_ $ do
    head_ $ do
      title_ (toHtml title)
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      xstaticScripts xfiles

    with body_ [class_ "font-mono cursor-default bg-stone-100 h-screen"] $ do
      with div_ [id_ "display-ws", class_ "h-full", makeAttribute "hx-ext" "ws", makeAttribute "ws-connect" "/ws/htmx"] $ do
        with div_ [id_ "w-0", class_ "h-full"] mempty
        forM_ mHtml id

mineSweeperApp :: App
mineSweeperApp =
  App
    { name = "minesweeper",
      tags = fromList ["Game"],
      description = "game",
      size = Just (240, 351),
      start = startMineSweeper
    }

handleEvent :: DisplayClient -> WinID -> MSEvent -> TVar MSState -> ProcessIO ()
handleEvent client wId msEv appStateV = do
  case msEv of
    NewGame -> atomically $ do
      -- writeTBQueue serviceQ StopTimer
      sendHtml client $ renderPanel wId appStateV (Just 0.0)
      sendHtml client $ renderSettings wId appStateV
    SettingsSelected level playerName boardColor -> do
      newBoard <- liftIO . initBoard $ levelToBoardSettings level
      hazard <- liftIO randomHazard
      atomically $ do
        modifyTVar' appStateV $ \s ->
          s
            { board = newBoard,
              state = Wait,
              settings = MSSettings level playerName boardColor hazard
            }
        sendHtml client $ renderApp wId appStateV
    SetFlagMode -> do
      atomically $ do
        appState <- readTVar appStateV
        case appState.state of
          Play st fm -> do
            modifyTVar' appStateV $ \s -> s {state = Play st (not fm)}
            sendHtml client $ renderFlag wId appStateV
          _ -> pure ()
    ClickCell cellCoord -> do
      atTime <- liftIO getCurrentTime
      appState' <- readTVarIO appStateV
      case countOpenCells appState'.board of
        0 -> do
          -- Ensure the first click on the board is not a hazard
          newBoard <- liftIO $ ensureNFBoard appState'.board cellCoord appState'.settings.level
          atomically $ modifyTVar' appStateV $ \s -> s {board = newBoard}
          pure ()
        _ -> pure ()
      case appState'.state of
        Wait -> atomically $ do
          modifyTVar' appStateV $ \s -> s {state = Play atTime False}
        -- writeTBQueue serviceQ StartTimer
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
                  -- writeTBQueue serviceQ StopTimer
                  atomically $ do
                    modifyTVar' appStateV $ \s ->
                      s
                        { board = openCell cellCoord appState.board,
                          state = Gameover
                        }
                    sendHtml client $ renderBoard wId appStateV
                    sendHtml client $ renderPanel wId appStateV (Just playDuration)
                False -> do
                  let gs1 = openCell cellCoord appState.board
                      gs2 = openAdjBlank0Cells (levelToBoardSettings appState.settings.level) cellCoord gs1
                  case countHiddenBlank gs2 == 0 of
                    True -> do
                      atomically $ do
                        -- writeTBQueue serviceQ StopTimer
                        modifyTVar' appStateV $ \s -> s {board = gs2, state = Win}
                        sendHtml client $ renderBoard wId appStateV
                        sendHtml client $ renderPanel wId appStateV (Just playDuration)
                    -- addScore dbConn appState.settings.playerName atTime playDuration appState.settings.level
                    -- leaderBoard <- renderLeaderBoard appStateV dbConn
                    -- pure [board, panel, leaderBoard]
                    False -> do
                      atomically $ do
                        modifyTVar' appStateV $ \s -> s {board = gs2}
                        sendHtml client $ renderBoard wId appStateV
                        sendHtml client $ renderSmiley wId appStateV
        Play _ True -> atomically $ do
          let board = setFlagOnCell cellCoord appState.board
          modifyTVar' appStateV $ \s -> s {board}
          sendHtml client $ renderFlag wId appStateV
          sendHtml client $ renderBoard wId appStateV
        _ -> pure ()
    _ -> pure ()
  where
    mkPlayDuration :: MSGameState -> UTCTime -> Float
    mkPlayDuration s curD = case s of
      Play startDate _ -> diffTimeToFloat curD startDate
      _ -> error "Should not happen"
    ensureNFBoard :: MSBoard -> MSCellCoord -> MSLevel -> IO MSBoard
    ensureNFBoard board cellCoord level = case isMineCell cellCoord board of
      True -> do
        newBoard <- initBoard $ levelToBoardSettings level
        ensureNFBoard newBoard cellCoord level
      False -> pure board

diffTimeToFloat :: UTCTime -> UTCTime -> Float
diffTimeToFloat a b = realToFrac $ diffUTCTime a b

startMineSweeper :: AppStart
startMineSweeper _clients wid pipeAE = do
  let level = defaultLevel
  board <- liftIO $ initBoard $ levelToBoardSettings level
  hazard <- liftIO randomHazard
  state <- newTVarIO $ MSState board Wait (MSSettings level "Anonymous" Blue hazard)
  forever $ do
    res <- atomically =<< waitTransaction 60000 (readPipe pipeAE)
    case res of
      WaitTimeout {} -> pure ()
      WaitCompleted (AppDisplay de) -> case de of
        UserConnected _ client -> atomically $ sendHtml client (renderApp wid state)
        _ -> pure ()
      WaitCompleted (AppTrigger ev) -> case ev of
        GuiEvent client tn td -> do
          appEventM <- toAppEvents tn td
          case appEventM of
            Just appEvent -> handleEvent client wid appEvent state
            _ -> pure ()
      WaitCompleted _ -> pure ()
  where
    toAppEvents :: TriggerName -> Value -> ProcessIO (Maybe MSEvent)
    toAppEvents tn td = case tn of
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
               td ^? key "playerName" . _String,
               td ^? key "boardColor" . _String
             ) of
          (Just level, Just playerName, Just boardColor) -> do
            pure . Just $ SettingsSelected (from level) playerName (from boardColor)
          _ -> pure Nothing
      _ -> do
        logInfo "Got unknown game event" ["TriggerName" .= tn]
        pure Nothing

withEvent :: Monad m => WinID -> Text -> [Attribute] -> HtmlT m () -> HtmlT m ()
withEvent wid tId tAttrs elm = with elm ([id_ (withWID wid tId), wsSend' ""] <> tAttrs)
  where
    wsSend' = makeAttribute "ws-send"

renderApp :: WinID -> TVar MSState -> HtmlT STM ()
renderApp wid state = do
  div_ [id_ (withWID wid "w"), class_ "w-60 border-2 border-gray-400 bg-gray-100"] $ do
    renderPanel wid state Nothing
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

renderSettings :: WinID -> TVar MSState -> HtmlT STM ()
renderSettings wid appStateV = do
  appState <- lift $ readTVar appStateV
  let playerName = appState.settings.playerName
      selectedLevel = appState.settings.level
  div_ [id_ "MSBoard"] $ do
    withEvent wid "setSettings" [] $ do
      form_ [class_ $ withThemeBgColor appState.settings.color "100" "flex flex-col items-center gap-px"] $ do
        div_ [class_ "my-2"] $ startButton appState
        label_ [class_ "m-1 font-semibold"] "Set the board color"
        colorInput appState.settings.color
        label_ [class_ "m-1 font-semibold"] "Set your name"
        nameInput playerName
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
    nameInput :: Text -> HtmlT STM ()
    nameInput playerName =
      input_
        [ type_ "text",
          name_ "playerName",
          value_ playerName,
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