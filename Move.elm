module Move where

import Isom(Isom)
import Isom
import PieceUtils as Piece
import Util(..)
import Piece
import Piece(Piece, ForATime)
import Signal
import Transform2D(Transform2D)
import Transform2D
import List
import Maybe
import Config

-- N-ary direct product for some N
type alias Move = List Isom
-- "List" could be an arbitrary Applicative Traversable

type alias SInterp = List Transform2D

interpret : Move -> (SInterp -> Piece ForATime (List Transform2D))
interpret m mInit =
  Piece.sequence
  <| List.map2 (|>) mInit
  <| List.map (Isom.interpret) m

sInterpret : Move -> SInterp
sInterpret = List.map Isom.sInterpret

sMultiply : SInterp -> SInterp -> SInterp
sMultiply = List.map2 Transform2D.multiply
