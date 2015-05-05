module GameTypes where

import Move
import Array exposing (Array)
import Move exposing (Move)
import Random

type alias Game = Array Level

type alias Level =
  { availableMoves : List Move
  , maxMoves       : Int
  , initial        : Move.SInterp
  , difficulty     : Difficulty
  }

type alias GameState =
  { levelState     : LevelState
  , currLevel      : Level
  , lastMove       : Maybe Move
  , totalScore     : Int
  , seed           : Random.Seed
  }

type Triggered
  = Have
  | Havent

type alias WinData = 
  { pre : Move.SInterp
  , move : Move
  , movesLeft : Int
  , totalScore : Int
  , difficulty : Difficulty
  }
type Ending
  = Win WinData
  | Lose
    { pre       : Move.SInterp
    , move      : Move
    , init      : Move.SInterp
    , maxMoves  : Int
    }

type EndState
  = Normal
  | End Ending Triggered

type alias LevelState =
  { movesLeft : Int
  , postMove  : Move.SInterp
  , preMove   : Move.SInterp
  , endState  : EndState
  }

type alias AnimState =
  { currTranses : Move.SInterp
  , movesLeft   : Int
  }

type Difficulty = S | M | L | XL

difficultyScore d = case d of
  S -> 100
  M -> 200
  L -> 300
  XL -> 400

type Update
  = Clicked Move
  | PlayLevelOfDifficulty Difficulty
  | SetTotalScore Int
  | Hovered Move
  | Unhovered
  | ResetLevel
  -- Hack
  | SetEndState EndState
  | NoOp

type Mode
  = TitleScreen
  | PlayLevel
  | ChooseLevel
  | Finished

