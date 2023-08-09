{-# HLINT ignore "Use if" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module HazardHunter (run, hazardHunterApp) where

import Butler
import Butler.App (Display (..), withEvent)
import Butler.Auth (PageDesc (PageDesc), PageTitle (..))
import Butler.Database (Database, NamedParam ((:=)), dbExecute, dbQuery, dbSimpleCreate, withDatabase)
import Butler.Display.Session (Session (..), UserName, changeUsername)
import Data.Aeson (Value (Number))
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, diffUTCTime, formatTime)
import HazardHunter.Engine
import Text.Printf (printf)
import Prelude

version :: Text
version = "1.0.3"

hazardHunterApp :: Database -> App
hazardHunterApp db =
    let name = "Hazard Hunter"
        tags = mempty
        title = "Hazard Hunter - Minesweeper like game - Find Hazards and Enter leaderboard !"
        description =
            "HazardHunter is a mini web game based on the legendary Minesweeper game's logic. "
                <> "Goal is to discover various Hazards by guessing their places on the board. "
                <> "Best scores are stored in the leaderboard !"
        size = Nothing
        xfiles = mempty
        start = startHH db
        acceptFiles = Nothing
        settings = mempty
        extraXfiles = mempty
     in App{..}

run :: IO ()
run =
    void $
        runMain $
            spawnInitProcess ".butler-storage" $
                withDatabase "leaderboard" migrations runApp
  where
    migrations = dbSimpleCreate "scores" "id INTEGER PRIMARY KEY, name TEXT, date DATE, duration REAL, level TEXT"
    runApp db =
        let app = hazardHunterApp db
         in serveApps (publicDisplayApp (PageTitle app.title) (Just $ PageDesc app.description)) [app]

addScore :: Database -> Text -> UTCTime -> Float -> MSLevel -> IO ()
addScore db name date duration level =
    dbExecute
        db
        "INSERT INTO scores (name, date, duration, level) VALUES (:name,:date,:duration,:level)"
        [":name" := name, ":date" := date, ":duration" := duration, ":level" := show level]

getTopScores :: Database -> Integer -> MSLevel -> IO [Score]
getTopScores db limit level =
    dbQuery
        db
        "SELECT * from scores WHERE level = :level ORDER BY duration ASC LIMIT :limit"
        [":level" := show level, ":limit" := show limit]

handleEvent ::
    UserName ->
    DisplayClients ->
    AppID ->
    MSEvent ->
    Database ->
    MemoryVar MSState ->
    ProcessIO ()
handleEvent username clients wId msEv db appStateV = do
    case msEv of
        NewGame -> do
            atomically $ modifyMemoryVar appStateV $ \s -> s{state = Wait}
            sendsHtml clients $ renderPanel wId appStateV (Just 0.0)
            sendsHtml clients $ renderSettings username wId appStateV
        SettingsSelected level boardColor -> do
            newBoard <- liftIO . initBoard $ levelToBoardSettings level
            hazard <- liftIO randomHazard
            atomically $ do
                modifyMemoryVar appStateV $ \s ->
                    s
                        { board = newBoard
                        , state = Wait
                        , HazardHunter.Engine.settings = MSSettings level boardColor hazard
                        }
            app <- liftIO $ renderApp wId db appStateV
            sendsHtml clients app
        SetFlagMode -> do
            join $ atomically $ do
                appState <- readMemoryVar appStateV
                case appState.state of
                    Play st fm -> do
                        modifyMemoryVar appStateV $ \s -> s{state = Play st (not fm)}
                        pure $ sendsHtml clients $ renderFlag wId appStateV
                    _ -> pure $ pure ()
        ClickCell cellCoord -> do
            atTime <- liftIO getCurrentTime
            appState' <- atomically $ readMemoryVar appStateV
            case countOpenCells appState'.board of
                0 -> do
                    newBoard <- liftIO $ ensureNFBoard appState'.board cellCoord appState'.settings.level
                    atomically $ modifyMemoryVar appStateV $ \s -> s{board = newBoard}
                    pure ()
                _ -> pure ()
            case appState'.state of
                Wait -> do
                    atomically $ do
                        modifyMemoryVar appStateV $ \s -> s{state = Play atTime False}
                _ -> pure ()
            appState <- atomically $ readMemoryVar appStateV
            case appState.state of
                Play _ False -> do
                    let playDuration = mkPlayDuration appState.state atTime
                    case isFlagCell cellCoord appState.board of
                        True -> pure ()
                        False -> do
                            case isMineCell cellCoord appState.board of
                                True -> do
                                    atomically $ do
                                        modifyMemoryVar appStateV $ \s ->
                                            s
                                                { board = openCell cellCoord appState.board
                                                , state = Gameover
                                                }
                                    sendsHtml clients $ renderBoard wId appStateV
                                    sendsHtml clients $ renderPanel wId appStateV (Just playDuration)
                                False -> do
                                    let gs1 = openCell cellCoord appState.board
                                        gs2 = openAdjBlank0Cells (levelToBoardSettings appState.settings.level) cellCoord gs1
                                    case countHiddenBlank gs2 == 0 of
                                        True -> do
                                            atomically $ do
                                                modifyMemoryVar appStateV $ \s -> s{board = gs2, state = Win}
                                            sendsHtml clients $ renderBoard wId appStateV
                                            sendsHtml clients $ renderPanel wId appStateV (Just playDuration)
                                            liftIO $ addScore db (from username) atTime playDuration appState.settings.level
                                            leaderBoard <- liftIO $ renderLeaderBoard wId appStateV db
                                            sendsHtml clients leaderBoard
                                        False -> do
                                            atomically $ do
                                                modifyMemoryVar appStateV $ \s -> s{board = gs2}
                                            sendsHtml clients $ renderBoard wId appStateV
                                            sendsHtml clients $ renderSmiley wId appStateV
                Play _ True -> do
                    atomically $ do
                        let board = setFlagOnCell cellCoord appState.board
                        modifyMemoryVar appStateV $ \s -> s{board}
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

startHH :: Database -> AppContext -> ProcessIO ()
startHH db ctx = do
    let level = defaultLevel
    board <- liftIO $ initBoard $ levelToBoardSettings level
    hazard <- liftIO randomHazard
    let msState = MSState board Wait (MSSettings level Blue hazard)
        memAddr = "hazard-hunter-" <> showT ctx.wid <> ".bin"
    (_, state) <- newProcessMemory (from memAddr) (pure msState)
    spawnThread_ $ asyncTimerUpdateThread state ctx.shared.clients
    forever $ do
        res <- atomically (readPipe ctx.pipe)
        case res of
            AppDisplay _ -> do
                app <- liftIO $ renderApp ctx.wid db state
                sendHtmlOnConnect app res
            AppTrigger ev -> do
                mAppEvent <- toAppEvents ev.client ev.trigger ev.body
                username <- readTVarIO (ev.client.session.username)
                case mAppEvent of
                    Just appEvent -> handleEvent username ctx.shared.clients ctx.wid appEvent db state
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
            case (td ^? key "cx" . _Integer, td ^? key "cy" . _Integer) of
                (Just cx, Just cy) ->
                    pure $
                        Just $
                            ClickCell
                                (MSCellCoord (fromInteger $ toInteger cx) (fromInteger $ toInteger cy))
                _ -> pure Nothing
        TriggerName "setSettings" -> do
            logInfo "Got <setSettings> game event" ["data" .= td]
            case ( td ^? key "level" . _String
                 , td ^? key "playerName" . _JSON
                 , td ^? key "boardColor" . _String
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
    asyncTimerUpdateThread :: MemoryVar MSState -> DisplayClients -> ProcessIO Void
    asyncTimerUpdateThread appStateV clients = forever $ do
        appState <- atomically $ readMemoryVar appStateV
        case appState.state of
            Play{} -> do
                atTime <- liftIO getCurrentTime
                let playDuration = mkPlayDuration appState.state atTime
                sendsHtml clients $ renderTimer playDuration
            _ -> pure ()
        sleep 990

renderApp :: AppID -> Database -> MemoryVar MSState -> IO (HtmlT STM ())
renderApp wid db appStateV = do
    leaderBoard <- liftIO $ renderLeaderBoard wid appStateV db
    appState <- atomically $ readMemoryVar appStateV
    pure $ div_ [id_ (withWID wid "w"), class_ "container mx-auto"] $ do
        div_ [id_ (withWID wid "w"), class_ "flex flex-row justify-center"] $ do
            div_ [id_ "MSMain", class_ "min-w-fit max-w-fit border-2 rounded border-gray-400 bg-gray-100"] $ do
                div_ [class_ "flex flex-col"] $ do
                    div_ [class_ "border-solid rounded border-2 m-1 border-gray-300"] $ do
                        renderPanel wid appStateV (Just 0.0)
                        renderBoard wid appStateV
                    div_ [class_ "border-solid rounded border-2 m-1 border-gray-300"] $ do
                        leaderBoard
                    div_ [class_ $ withThemeBgColor appState.settings.color "200" ""] $ do
                        div_ [class_ "flex flex-row gap-2 flex-row-reverse pr-2"] $ do
                            div_ [] (toHtml version)
                            a_
                                [ class_ "text-blue-600"
                                , href_ "https://github.com/web-apps-lab/HazardHunter"
                                ]
                                "HazardHunter"

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

renderFlag :: AppID -> MemoryVar MSState -> HtmlT STM ()
renderFlag wid appStateV = do
    appState <- lift $ readMemoryVar appStateV
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

renderSmiley :: AppID -> MemoryVar MSState -> HtmlT STM ()
renderSmiley wid appStateV = do
    appState <- lift $ readMemoryVar appStateV
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

renderPanel :: AppID -> MemoryVar MSState -> Maybe Float -> HtmlT STM ()
renderPanel wid appStateV durationM = do
    let smiley = renderSmiley wid appStateV
        flag = renderFlag wid appStateV
    appState <- lift $ readMemoryVar appStateV
    div_ [id_ "MSPanel", class_ $ withThemeBgColor appState.settings.color "200" "flex justify-between"] $ do
        let mineCount' = mineCount $ levelToBoardSettings appState.settings.level
        div_ [class_ "pl-1 w-24"] $ toHtml $ hazardLabel mineCount' appState.settings.hazard
        div_ [class_ "flex flex-row gap-2"] flag
        forM_ durationM renderTimer
        smiley
  where
    hazardLabel :: Int -> Hazard -> Text
    hazardLabel count hazard = from (show count) <> " " <> hazardToText hazard

renderBoard :: AppID -> MemoryVar MSState -> HtmlT STM ()
renderBoard wid appStateV = do
    appState <- lift $ readMemoryVar appStateV
    let sizeCount' = sizeCount $ levelToBoardSettings appState.settings.level
    let gridType = "grid-cols-[" <> T.intercalate "_" (Prelude.replicate (sizeCount' + 1) "19px") <> "]"
    div_ [id_ "MSBoard"] $ do
        div_ [class_ "flex place-content-center m-1"] $ do
            div_ [class_ $ "grid gap-1 " <> gridType] $ do
                mapM_ (renderCell appState.state appState.settings.hazard appState.settings.color) $
                    Map.toList appState.board
  where
    renderCell :: MSGameState -> Hazard -> Color -> (MSCellCoord, MSCell) -> HtmlT STM ()
    renderCell gameState hazard color (cellCoords, cellState) =
        let cellId =
                [ ("cx", Number $ fromInteger $ toInteger cellCoords.cx)
                , ("cy", Number $ fromInteger $ toInteger cellCoords.cy)
                ]
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
        installCellEvent :: MSGameState -> [Pair] -> HtmlT STM () -> HtmlT STM ()
        installCellEvent gs cellId elm =
            let elm' = withEvent wid "clickCell" cellId elm
             in case gs of
                    Play _ _ -> elm'
                    Wait -> elm'
                    _ -> elm

renderSettings :: UserName -> AppID -> MemoryVar MSState -> HtmlT STM ()
renderSettings username wid appStateV = do
    appState <- lift $ readMemoryVar appStateV
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
            [ class_ "block mb-2 text-center border border-slate-300 rounded-md"
            , name_ "boardColor"
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
            [ type_ "text"
            , name_ "playerName"
            , value_ (into @Text playerName)
            , placeholder_ "Anonymous"
            , size_ "15"
            , maxlength_ "15"
            , class_ "h-8 text-center border border-slate-300 rounded-md focus:border-slate-400"
            ]
    startButton :: MSState -> HtmlT STM ()
    startButton appState =
        button_
            [ type_ "submit"
            , class_ $ withThemeBorderColor appState.settings.color "300" "p-1 border-4 rounded"
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

renderLeaderBoard :: AppID -> MemoryVar MSState -> Database -> IO (HtmlT STM ())
renderLeaderBoard _wid appStateV db = do
    appState <- atomically $ readMemoryVar appStateV
    scores <- getTopScores db 25 appState.settings.level
    pure $
        div_ [id_ "MSLeaderBoard", class_ $ withThemeBgColor appState.settings.color "100" ""] $
            case length scores of
                0 -> p_ "The leaderboard is empty. Be the first to appear here !"
                _ -> mapM_ displayScoreLine $ zip [1 ..] scores
  where
    displayScoreLine :: (Int, Score) -> HtmlT STM ()
    displayScoreLine (index, Score{..}) = do
        let bgColor = case index of
                1 -> "pt-1 pb-1 font-bold text-yellow-800"
                2 -> "pt-1 pb-1 font-semibold text-yellow-700"
                3 -> "pt-1 pb-1 font-semibold text-yellow-600"
                _ -> ""
        div_ [class_ $ "flex " <> bgColor] $ do
            div_ [class_ "w-1/12 pl-1 text-left"] $ toHtml $ show index
            div_ [class_ "w-4/12"] $ toHtml $ formatTime defaultTimeLocale "%F" scoreDate
            div_ [class_ "w-5/12"] $ toHtml scoreName
            div_ [class_ "w-2/12 pr-1 text-right"] $ toHtml (toDurationT scoreDuration)
