module Lev1 where

import Level
import Transform2D(..)
import Move(Move(..))

level1 =
  { maxMoves       = 3
  , availableMoves =
    [ Reflection (pi / 2)
    , Reflection 0
    , Rotation (2 * pi / 10)
    ]
  , goal           = multiply (rotation (4 * 2 * pi / 10)) (translation 100 0)
  , initialTrans   = translation 100 0
  }

main = Level.run level1
