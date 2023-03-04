{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module HazardHunter.Engine where

import Butler.Prelude
import qualified Data.Map as Map
import System.Random (randomRIO)
import Prelude

data MSState = MSState
  { board :: MSBoard,
    state :: MSGameState,
    settings :: MSSettings
  }
  deriving (Show, Generic)

instance Serialise MSState

data MSSettings = MSSettings
  { level :: MSLevel,
    color :: Color,
    hazard :: Hazard
  }
  deriving (Show, Generic)

instance Serialise MSSettings

data MSBoardSettings = MSBoardSettings
  { sizeCount :: Int,
    mineCount :: Int
  }

data MSGameState
  = Play UTCTime Bool
  | Win
  | Gameover
  | Wait
  deriving (Show, Generic)

instance Serialise MSGameState

data MSCellContent
  = Mine
  | Blank Int
  deriving (Show, Generic)

instance Serialise MSCellContent

data MSCellStatus
  = Open
  | Hidden Bool
  deriving (Show, Generic)

instance Serialise MSCellStatus

data MSCell = MSCell
  { cellContent :: MSCellContent,
    cellStatus :: MSCellStatus
  }
  deriving (Show, Generic)

instance Serialise MSCell

data MSCellCoord = MSCellCoord
  { cx :: Int,
    cy :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance Serialise MSCellCoord

type MSBoard = Map.Map MSCellCoord MSCell

data MSEvent
  = NewGame
  | ClickCell MSCellCoord
  | SettingsSelected MSLevel Color
  | SetFlagMode

data MSLevel
  = Baby
  | Beginner
  | Intermediate
  | Expert
  | Specialist
  | Survivalist
  deriving (Bounded, Eq, Enum, Show, Generic)

instance Serialise MSLevel

instance From Text MSLevel where
  from txt = case txt of
    "Baby" -> Baby
    "Beginner" -> Beginner
    "Intermediate" -> Intermediate
    "Expert" -> Expert
    "Specialist" -> Specialist
    "Survivalist" -> Survivalist
    _ -> error "Unhandled level"

data Hazard
  = HMine
  | HSnake
  | HSpidder
  | HPumpkin
  | HPoo
  | HVampire
  | HTengu
  | HAlien
  | HAlien2
  | HGost
  deriving (Bounded, Enum, Show, Generic)

instance Serialise Hazard

hazards :: [Hazard]
hazards = [minBound .. maxBound]

randomHazard :: IO Hazard
randomHazard = do
  selected <- randomRIO (0, length hazards - 1)
  pure $ hazards !! selected

hazardToText :: Hazard -> Text
hazardToText hazard = case hazard of
  HMine -> "💣"
  HSnake -> "🐍"
  HSpidder -> "🕷"
  HPumpkin -> "🎃"
  HPoo -> "💩"
  HVampire -> "🧛"
  HTengu -> "👺"
  HAlien -> "👽"
  HAlien2 -> "👾"
  HGost -> "👻"

defaultLevel :: MSLevel
defaultLevel = Beginner

levelToBoardSettings :: MSLevel -> MSBoardSettings
levelToBoardSettings level = case level of
  Baby -> MSBoardSettings 6 3
  Beginner -> MSBoardSettings 9 9
  Intermediate -> MSBoardSettings 15 30
  Expert -> MSBoardSettings 15 50
  Specialist -> MSBoardSettings 15 60
  Survivalist -> MSBoardSettings 15 70

data Color
  = Blue
  | Pink
  | Green
  deriving (Show, Eq, Enum, Bounded, Generic)

instance Serialise Color

instance From Text Color where
  from colorText = case colorText of
    "Blue" -> Blue
    "Green" -> Green
    "Pink" -> Pink
    _ -> Blue

initBoard :: MSBoardSettings -> IO MSBoard
initBoard settings@MSBoardSettings {..} = do
  let cellsCoords = [MSCellCoord x y | x <- [0 .. sizeCount], y <- [0 .. sizeCount]]
      blankBoard = Map.fromList $ map (\coord -> (coord, MSCell (Blank 0) (Hidden False))) cellsCoords
  minesCoords <- getMinesCoords cellsCoords []
  pure $ setBoard blankBoard minesCoords
  where
    getMinesCoords :: [MSCellCoord] -> [MSCellCoord] -> IO [MSCellCoord]
    getMinesCoords availableCellsCords minesCoords = do
      if length minesCoords == mineCount
        then pure minesCoords
        else do
          selectedIndex <- randomRIO (0, length availableCellsCords - 1)
          let selectedCoord = availableCellsCords !! selectedIndex
              remainingCellsCords = filter (/= selectedCoord) availableCellsCords
          getMinesCoords remainingCellsCords (minesCoords <> [selectedCoord])
    setBoard :: MSBoard -> [MSCellCoord] -> MSBoard
    setBoard board minesCoords =
      let adjCellds = concatMap (getAdjCellCoords settings) minesCoords
          board' = installAdjCells board adjCellds
       in installMines board' minesCoords
      where
        installMines :: MSBoard -> [MSCellCoord] -> MSBoard
        installMines b cs = case cs of
          [] -> b
          [x] -> Map.insert x (MSCell Mine (Hidden False)) b
          (x : xs) -> installMines (Map.insert x (MSCell Mine (Hidden False)) b) xs
        installAdjCells :: MSBoard -> [MSCellCoord] -> MSBoard
        installAdjCells b cs = case cs of
          [] -> b
          [x] -> installAdjCell b x
          (x : xs) -> installAdjCells (installAdjCell b x) xs
        installAdjCell :: MSBoard -> MSCellCoord -> MSBoard
        installAdjCell b c =
          Map.insertWith
            ( \_ oldv ->
                case oldv of
                  MSCell (Blank v) s -> MSCell (Blank (v + 1)) s
                  other -> other
            )
            c
            (MSCell (Blank 1) (Hidden False))
            b

getAdjCellCoords :: MSBoardSettings -> MSCellCoord -> [MSCellCoord]
getAdjCellCoords MSBoardSettings {..} MSCellCoord {..} =
  let isInBoard (MSCellCoord cx' cy') =
        cx' >= 0
          && cx' <= sizeCount
          && cy' >= 0
          && cy' <= sizeCount
   in filter
        isInBoard
        [ MSCellCoord (cx - 1) (cy - 1),
          MSCellCoord (cx - 1) cy,
          MSCellCoord (cx - 1) (cy + 1),
          MSCellCoord (cx + 1) (cy - 1),
          MSCellCoord (cx + 1) cy,
          MSCellCoord (cx + 1) (cy + 1),
          MSCellCoord cx (cy - 1),
          MSCellCoord cx (cy + 1)
        ]

openCell :: MSCellCoord -> MSBoard -> MSBoard
openCell = Map.update func
  where
    func :: MSCell -> Maybe MSCell
    func (MSCell content _) = Just $ MSCell content Open

setFlagOnCell :: MSCellCoord -> MSBoard -> MSBoard
setFlagOnCell = Map.update func
  where
    func :: MSCell -> Maybe MSCell
    func (MSCell content (Hidden flagState)) = Just $ MSCell content (Hidden (not flagState))
    func cell = Just cell

getCell :: MSCellCoord -> MSBoard -> Maybe MSCell
getCell = Map.lookup

isBlank0Cell :: MSCellCoord -> MSBoard -> Bool
isBlank0Cell cellCoord board = case getCell cellCoord board of
  Just (MSCell (Blank 0) _) -> True
  _ -> False

isHiddenCell :: MSCellCoord -> MSBoard -> Bool
isHiddenCell cellCoord board = case getCell cellCoord board of
  Just (MSCell _ (Hidden False)) -> True
  _ -> False

isMineCell :: MSCellCoord -> MSBoard -> Bool
isMineCell cellCoord board = case getCell cellCoord board of
  Just (MSCell Mine _) -> True
  _ -> False

isFlagCell :: MSCellCoord -> MSBoard -> Bool
isFlagCell cellCoord board = case getCell cellCoord board of
  Just (MSCell _ (Hidden True)) -> True
  _ -> False

countHiddenBlank :: MSBoard -> Int
countHiddenBlank board = length (filter keepHiddenBlank (Map.elems board))
  where
    keepHiddenBlank (MSCell (Blank _) (Hidden False)) = True
    keepHiddenBlank _ = False

countFlagCells :: MSBoard -> Int
countFlagCells board = length (filter keepFlagCell (Map.elems board))
  where
    keepFlagCell (MSCell _ (Hidden True)) = True
    keepFlagCell _ = False

countOpenCells :: MSBoard -> Int
countOpenCells board = length (filter keepOpenCell (Map.elems board))
  where
    keepOpenCell (MSCell _ Open) = True
    keepOpenCell _ = False

openAdjBlank0Cells :: MSBoardSettings -> MSCellCoord -> MSBoard -> MSBoard
openAdjBlank0Cells settings cellCoord board =
  if isBlank0Cell cellCoord board
    then openCells (getAdjCellCoords settings cellCoord) board
    else board
  where
    openCells :: [MSCellCoord] -> MSBoard -> MSBoard
    openCells cellsCoords b = case cellsCoords of
      [] -> b
      [x] -> openCell' x b
      (x : xs) -> openCells xs $ openCell' x b
    openCell' coord b =
      if isHiddenCell coord b
        then let nb = openCell coord b in openAdjBlank0Cells settings coord nb
        else b
