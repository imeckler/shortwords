module Main where

import Signal
import Array
import Util(..)
import List
import Level
import Transform2D as T
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
  let m0 = [I.translation (-100, 0), I.translation (-100, 0)]
      m1 = [I.translation (0, -100), I.translation (0, 100)]
      m2 = [I.rotation (pi / 2), I.rotation (pi)]
      m3 = [I.reflection (3*pi/4), I.identity]
  in
  { maxMoves = 3
  , availableMoves = [m1,m2,m0,m3]
  , initial =
    salted (T.rotation (pi/2)) [m0,m3,m1]
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
    List.map (\m -> T.multiply m (T.translation -50 -50))
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
    [ T.identity
    , List.foldl multiply T.identity
      (List.map I.sInterpret [ ref, rot, ref, trans, ref, rot, ref ])
    ]
  }

salted salt =
  List.map (\m -> T.multiply m salt)
  << List.foldr1 M.sMultiply
  << List.map invMove
  << List.reverse

invMove = List.map invert << M.sInterpret

(<>) = T.multiply

easy1 = 
  let  m0 = [ I.reflection (2*pi/3), I.rotation (2*pi/3) ] 
       m1 = [ I.translation (-100, 0), I.rotation (2*pi/3) ] 
       m2 = [ I.reflection 0, I.rotation (2*pi/3) ] 
  in
  { maxMoves = 3
  , availableMoves = [m0,m1,m2]
  , initial = salted (T.translation -50 50 <> T.rotation (pi/3) <> reflection (pi/2))
      [m2, m0, m1]
  }

veryEasy1 =
  let m0 = [ I.reflection (pi/2), I.identity ]
      m1 = [ I.translation (0,-100), I.identity ]
  in
  { maxMoves = 2
  , availableMoves = [m0,m1]
  , initial = salted (T.translation -100 0) [m0,m1]
  }

-- both are solutions!
veryEasy2 =
  let m0 = [ I.reflection (pi/2), I.identity ]
      m1 = [ I.identity, I.rotation (2*pi/3) ]
  in
  { maxMoves       = 2
  , availableMoves = [m0, m1]
  , initial        = salted (T.translation -100 100 <> reflection (pi/2)) [m0,m1]
  }

{-
parity =
  let m0 = [ reflection pi 

twoTorsion =
  let m0 = [ , I.identity ]
      m1 = [ , I.rotation pi ]
      m0 = [ , I.identity ]
      m1 = [ , I.rotation pi ]
-}
game = Array.fromList
  [ veryEasy1
  , veryEasy2
  , level1
  , easy1
  , level2
  , level7moves
  , tripleAction1
  ]
--, level2, level1]

main = Level.run setHighestLevel setLocalStorageChan game

-- BAD stuff
setLocalStorageChan : Signal.Channel Int
setLocalStorageChan = Signal.channel 0

port setLocalStorage : Signal Int
port setLocalStorage = Signal.subscribe setLocalStorageChan

port setHighestLevel : Signal Int

