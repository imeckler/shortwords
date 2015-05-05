module Main where

import Signal
import Array
import Util exposing (..)
import List
import Level
import Transform2D as T
import Transform2D exposing (..)
import Isom as I
import Move as M
import Ratio as R
import Task exposing (Task)

reflection a =
  multiply (rotation a)
    (multiply (scaleY -1) (rotation -a))

salted salt =
  List.map (\m -> T.multiply m salt)
  << foldr1 M.sMultiply
  << List.map invMove
  << List.reverse

invMove = List.map invert << M.sInterpret

(<>) = T.multiply

easy1 = 
  let  m0 = [ I.reflection (2*pi/3), I.rotation (2 `R.over` 3) ] 
       m1 = [ I.translation (-100, 0), I.rotation (2 `R.over` 3) ] 
       m2 = [ I.reflection 0, I.rotation (2 `R.over` 3) ] 
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
      m1 = [ I.identity, I.rotation (2 `R.over` 3) ]
  in
  { maxMoves       = 2
  , availableMoves = [m0, m1]
  , initial        = salted (T.translation -100 100 <> reflection (pi/2)) [m0,m1]
  }

(ends_, sets_, game) = Level.run setTotalScore setLocalStorageChan.address

port ends : Signal (Task x ())
port ends = ends_

port sets : Signal (Task x ())
port sets = sets_

main = game

-- BAD stuff
setLocalStorageChan : Signal.Mailbox Int
setLocalStorageChan = Signal.mailbox 0

port setLocalStorage : Signal Int
port setLocalStorage = setLocalStorageChan.signal

port setTotalScore : Signal Int

