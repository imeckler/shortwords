module Generate where

import String
import Text
import List
import List((::))
import Transform2D as T
import Isom as I
import Util(..)
import Move as M
import Iterator
import Array
import Random
import Maybe
import Ratio
import GameTypes(Difficulty(..))
import Debug

rmap : (a -> b) -> Random.Generator a -> Random.Generator b
rmap f g = Random.customGenerator (\s -> let (x, s') = Random.generate g s in (f x, s'))

randThen : Random.Generator a -> (a -> Random.Generator b) -> Random.Generator b
randThen g f = Random.customGenerator (\s ->
  let (x, s') = Random.generate g s in Random.generate (f x) s')

rreturn x = Random.customGenerator (\s -> (x, s))

randomSInterpOfLength : Int -> List M.SInterp -> Random.Generator (M.SInterp)
randomSInterpOfLength =
  \n gens ->
    let gensArr = Array.fromList gens in
    Random.list n (rmap (\i -> case Array.get i gensArr of Just x -> x)
      (Random.int 0 (Array.length gensArr - 1)))
    |> rmap (Debug.log "randomSInterpOfLength" >> List.foldl1 M.sMultiply >> List.map invert)

sInterpsUpTo : Int -> List M.SInterp -> Iterator.Iterator M.SInterp
sInterpsUpTo n gens =
  let gens' : Iterator.Iterator M.SInterp
      gens' = Iterator.fromList gens
      go k =
        if | k == 0 -> Iterator.upTil 1 (\_ -> List.repeat 10 T.identity)
           | otherwise ->
             Iterator.concatMap (\m -> Iterator.map (M.sMultiply m) gens')
               (go (k - 1))
  in
  go n

isHardOfLength : Int -> List M.SInterp -> M.SInterp -> Bool
isHardOfLength n gens m =
  Iterator.all (\i ->
    Iterator.all (\m' -> not (close m m')) (sInterpsUpTo i gens))
    (Iterator.range 0 (n - 1))

randomTrans = rmap I.sInterpret randomIsom

{-
randomTrans =
  Random.float 0 (2 * pi)      `randThen` \r ->
  Random.int 0 1               `randThen` \si ->
  let ref = toFloat (1 - 2 * si) in
  rdup (Random.float -100 100) `randThen` \(x, y) ->
  rreturn (T.translation x y `T.multiply` T.rotation r `T.multiply` T.scaleY ref)
-}
timeout = 1000

-- Given gens g1 ... gk
-- want word (g_i1 ... g_in)^{-1} (initial state)
-- such that g_i1...gin /= g_j1..g_jm for m < n

hardSInterpOfLength : Int -> List M.SInterp -> Random.Generator (Maybe M.SInterp)
hardSInterpOfLength n gens =
  -- this is wrong. the salt can make it easy. E.g., gens are a, b.
  -- Target is aB. If salt is A, then we just have the undo B.
  -- Also, should forbid solutiont which are aaa (i.e., which are all the same trans). 
  let salted k m = rreturn (Just m)
    {-
        if | k == 0 -> rreturn Nothing
           | otherwise ->
             randomTrans `randThen` \salt -> let saltedm = M.sMultiply m (List.repeat 10 salt) in
               if allTogether saltedm then salted (k - 1) m else rreturn (Just saltedm) -}
-- hard/impossible level: MLLMSSMM
      go k =
        if k == 0
        then rreturn Nothing
        else 
          randomSInterpOfLength n gens `randThen` \m ->
            if isHardOfLength n gens m && not (close m (List.repeat 3 T.identity))
            then salted k m
            else go (k - 1)
  in
  go timeout


trans2Dclose a b = distTransform2D a b < 0.01
close xs ys = and (List.map2 trans2Dclose xs ys)

rdup : Random.Generator x -> Random.Generator (x, x)
rdup g = Random.pair g g

randomSign = let interp x = if x > 0 then identity else negate in 
  rmap ((\x -> 2 * x - 1) >> interp) (Random.int 0 1)

randomElem =
  let getNth xs n = case xs of x :: xs -> if n == 0 then x else getNth xs (n - 1) in
  \xs -> let n = List.length xs in rmap (getNth xs) (Random.int 0 (n - 1))

randomIsom =
  let randomTranslation =
        randomElem [2, 3, 4, 8]  `randThen` \denom ->
        Random.int 0 (denom - 1) `randThen` \num ->
        Random.int 1 3           `randThen` \scale ->
        let a = 2 * pi * (toFloat num / toFloat denom)
            r = toFloat (25 * (1 + scale))
        in
        rreturn <| I.Translation (r * cos a, r * sin a)

      randomRotation =
        randomElem [2, 3, 4, 8]  `randThen` \denom ->
        Random.int 1 (denom - 1) `randThen` \num ->
        rreturn (I.rotation (num `Ratio.over` denom))
        {-
        Random.int 2 8 `randThen` \denom ->
          rmap (\numi -> 
            let num = if numi >= 0 then numi + 1 else numi in
            I.rotation (num `Ratio.over` denom))
            (Random.int (-denom // 2) (denom // 2 - 1))
-}
      randomReflection =
        randomElem [2, 3, 4, 8]  `randThen` \denom ->
        Random.int 0 (denom - 1) `randThen` \num ->
        rreturn (I.reflection <| 2 * pi * toFloat num / toFloat denom)
{-
        Random.int 2 8 `randThen` \denom ->
          rmap (\numi -> 
            let num = if numi >= 0 then numi + 1 else numi in
            I.reflection <| toFloat num / toFloat denom)
            (Random.int  (-denom // 2) (denom // 2))
            -}
  in
  Random.int 0 3 `randThen` \i -> case i of
    0 -> randomTranslation
    1 -> randomRotation
    2 -> randomReflection
    3 -> rreturn I.identity

randomMove p = Random.list p randomIsom

-- difficulty is between 0 and 3
randomLevel difficulty =
  let (minMoves, maxMoves, minGens, maxGens, minSheets, maxSheets) = case difficulty of
        S  -> (2, 3, 2, 3, 2, 2)
        M  -> (3, 3, 2, 3, 2, 2)
        L  -> (3, 5, 3, 4, 2, 2)
        XL -> (4, 7, 3, 4, 2, 3)
  in
  Random.int minMoves maxMoves `randThen` \numMoves ->
  Random.int minGens maxGens `randThen` \numGens ->
  Random.int minSheets maxSheets `randThen` \numSheets ->
    Random.list numGens (randomMove numSheets) `randThen` \gens ->
    let gens' = List.filter (not << List.all ((==) I.identity)) gens in
    hardSInterpOfLength numMoves (List.map M.sInterpret gens') `randThen` \mtrans ->
      case mtrans of
        Nothing -> randomLevel difficulty
        Just x -> rreturn
          { availableMoves = gens'
          , initial        = List.map invert x
          , maxMoves       = numMoves
          , difficulty     = difficulty
          }

b1 = fst <| Random.generate (Random.list 3 (randomMove 2)) (Random.initialSeed 32)

