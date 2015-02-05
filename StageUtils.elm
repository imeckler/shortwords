module StageUtils where

import List
import Stage
import Stage.Internal(..)

durMin d1 d2 = case (d1, d2) of
  (Forever, _) -> d2
  (_, Forever) -> d1
  (ForATime t1, ForATime t2) -> ForATime (min t1 t2)

-- errr. how to do empty list case. that's a doozy.
-- could fix with explicit proxy for t
sequence : List (Stage t a) -> Stage t (List a)
sequence ss =
  let (Stage d0 _ :: ss') = ss in
  Stage (List.foldl (\(Stage d _) r -> durMin d r) d0 ss')
    (\t -> List.map (\(Stage _ f) -> f t) ss)

mapM f = sequence << List.map f
