module GameTypes where

import Move
import Move(Move)

type alias Game = List Level

type alias Level =
  { availableMoves : List Move
  , maxMoves       : Int
  , initial        : Move.SInterp
  , goal           : Move.SInterp
  }

type alias GameState =
  { levelState : LevelState
  , currLevel  : Level
  , rest       : List Level
  , finished   : Bool
  , lastMove  : Maybe Move
  }

type Triggered
  = Have
  | Havent

type Ending
  = Win {pre : Move.SInterp, move : Move, movesLeft : Int}
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

type Update
  = Clicked Move
  | NextLevel
  | Hovered Move
  | Unhovered
  -- Hack
  | SetEndState EndState
  | NoOp

type Mode
  = TitleScreen
  | PlayLevel
  | Finished

