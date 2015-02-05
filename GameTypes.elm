module GameTypes where

import Move
import Move(Move)

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
  }

type alias LevelState =
  { movesLeft : Int
  , postMove  : Move.SInterp
  , preMove   : Move.SInterp
  , hasWon    : Bool
  , justLost  : Bool
  }

type AnimState =
  { currTranses : Move.SInterp
  , movesLeft   : Int
  }

