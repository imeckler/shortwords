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
    |> rmap (List.foldl1 M.sMultiply >> List.map invert)

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

randomTrans =
  Random.float 0 (2 * pi)      `randThen` \r ->
  Random.int 0 1               `randThen` \si ->
  let ref = toFloat (1 - 2 * si) in
  rdup (Random.float -100 100) `randThen` \(x, y) ->
  rreturn (T.translation x y `T.multiply` T.rotation r `T.multiply` T.scaleY ref)

timeout = 1000

hardSInterpOfLength : Int -> List M.SInterp -> Random.Generator (Maybe M.SInterp)
hardSInterpOfLength n gens =
  let go k =
    if k == 0
    then rreturn Nothing
    else 
      randomSInterpOfLength n gens `randThen` \m ->
        if isHardOfLength n gens m
        then rmap (\salt -> Just (M.sMultiply m (List.repeat 10 salt))) randomTrans
        else go (k - 1)
  in
  go timeout


trans2Dclose a b = distTransform2D a b < 0.01
close xs ys = and (List.map2 trans2Dclose xs ys)

rdup : Random.Generator x -> Random.Generator (x, x)
rdup g = Random.pair g g

randomIsom =
  let randomTranslation =
        rmap I.Translation (rdup (Random.float -100 100))
      randomRotation =
        Random.int 2 8 `randThen` \denom ->
          rmap (\numi -> 
            -- let num = if numi >= 0 then num + 1 else numi in
            let num = if numi >= 0 then numi + 1 else numi in
            I.rotation (num `Ratio.over` denom))
            (Random.int (-denom // 2) (denom // 2))
      randomReflection =
        Random.int 2 8 `randThen` \denom ->
          rmap (\numi -> 
            let num = if numi >= 0 then numi + 1 else numi in
            I.reflection <| toFloat num / toFloat denom)
            (Random.int  (-denom // 2) (denom // 2))
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
          , initial        = x
          , maxMoves       = numMoves
          , difficulty     = difficulty
          }

b1 = fst <| Random.generate (Random.list 3 (randomMove 2)) (Random.initialSeed 32)

