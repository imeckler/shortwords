module Lev1 where

import List
import Level
import Transform2D
import Transform2D(..)
import Isom as I

reflection a =
  multiply (rotation a)
    (multiply (scaleY -1) (rotation -a))

level1 =
  { maxMoves       = 3
  , availableMoves =
    [ [I.reflection (pi / 2), I.identity]
    , [I.reflection 0, I.identity]
    , [I.rotation (2 * pi / 10), I.identity]
    ]
  , goal    = [ multiply (rotation (4 * 2 * pi / 10)) (translation 100 0)
              , multiply (rotation (4 * 2 * pi / 10)) (translation 100 0)
              ]
  , initial = [ translation 100 0
              , multiply (rotation (4 * 2 * pi / 10)) (translation 100 0)
              ]
  }

level2 =
  { maxMoves = 3
  , availableMoves =
    [ [I.translation (-100, 0), I.translation (-100, 0)]
    , [I.translation (0, -100), I.translation (0, 100)]
    , [I.rotation (pi / 2), I.rotation (pi)]
    , [I.reflection (3*pi/4), I.identity]
    ]
  , goal = List.repeat 2 (multiply (translation 0 100) (rotation (-3*pi/4)))
  , initial =
    [ multiply (translation 0 100) (reflection pi)
    , multiply (translation 0 -100) (rotation (-pi/2))
    ]
  }

level3 =
  let rot   = I.rotation (pi/6)
      ref   = I.reflection (pi/2)
      trans = I.translation (100,0)
  in
  { maxMoves = 7
  , availableMoves =
    [ [rot, I.identity]
    , [trans, I.identity]
    , [ref, I.identity]
    ]
  , goal = [Transform2D.identity, Transform2D.identity]
  , initial =
    [ Transform2D.identity
    , List.foldl multiply Transform2D.identity
      (List.map I.sInterpret [ ref, rot, ref, trans, ref, rot, ref ])
    ]
  }

game = [level1, level2, level3]--, level2, level1]

main = Level.run game
