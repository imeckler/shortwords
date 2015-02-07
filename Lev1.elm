module Lev1 where

import Level
import Transform2D
import Transform2D(..)
import Isom(Isom(..))

level1 =
  { maxMoves       = 3
  , availableMoves =
    [ [Reflection (pi / 2), Identity]
    , [Reflection 0, Identity]
    , [Rotation (2 * pi / 10), Identity]
    ]
  , goal    = [ multiply (rotation (4 * 2 * pi / 10)) (translation 100 0)
              , multiply (rotation (4 * 2 * pi / 10)) (translation 100 0)
              ]
  , initial = [ translation 100 0
              , multiply (rotation (4 * 2 * pi / 10)) (translation 100 0)
              ]
  }

level2 =
  { maxMoves = 4
  , availableMoves =
    [ [Translate (-100, 0), Translate (-100, 0)]
    , [Rotation pi, Rotation pi]
    , [Reflection (pi / 2), Identity]
    ]
  , goal = [ rotation (pi / 2), rotation (pi / 2) ]
  , initial = [ translation 0 100, translation 0 -100 ]
  }

game = [level2, level1]

main = Level.run game
