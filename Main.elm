module Main where

import Util(..)
import List
import Level
import Transform2D
import Transform2D(..)
import Isom as I
import Move as M

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
  , initial =
    [ multiply (translation 0 100) (reflection pi)
    , multiply (translation 0 -100) (rotation (-pi/2))
    ]
  }

tripleAction1 =
  let a = [I.identity, I.translation (100, 100), I.translation (-100, -100)]
      b = [I.rotation (pi/3), I.reflection (3*pi/4), I.identity]
      c = [I.reflection 0, I.identity, I.rotation (-pi/3)]
      [a',b',c'] = List.map (List.map invert << M.sInterpret) [a,b,c]
  in
  { maxMoves = 4
  , availableMoves = [a, c, b]
  , initial = 
    List.map (\m -> Transform2D.multiply m (Transform2D.translation -50 -50))
      (List.foldr1 M.sMultiply [c',b',a',b'])
  }
-- c b a b


level7moves =
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
  , initial =
    [ Transform2D.identity
    , List.foldl multiply Transform2D.identity
      (List.map I.sInterpret [ ref, rot, ref, trans, ref, rot, ref ])
    ]
  }

game = [level1, level2, tripleAction1, level7moves]--, level2, level1]

main = Level.run game
