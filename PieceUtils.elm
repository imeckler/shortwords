module PieceUtils where

import List
import Piece
import Piece.Internal(..)

durMin d1 d2 = case (d1, d2) of
  (Forever, _) -> d2
  (_, Forever) -> d1
  (ForATime t1, ForATime t2) -> ForATime (min t1 t2)

-- errr. how to do empty list case. that's a doozy.
-- could fix with explicit proxy for t
sequence : List (Piece t a) -> Piece t (List a)
sequence ss =
  let (Piece d0 _ :: ss') = ss in
  Piece (List.foldl (\(Piece d _) r -> durMin d r) d0 ss')
    (\t -> List.map (\(Piece _ f) -> f t) ss)

mapM f = sequence << List.map f
