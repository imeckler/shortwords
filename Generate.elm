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
import Config

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
    |> rmap (List.foldl1 M.sMultiply)

sInterpsUpTo : Int -> List M.SInterp -> Iterator.Iterator M.SInterp
sInterpsUpTo n gens =
  let gens' : Iterator.Iterator M.SInterp
      gens' = Iterator.fromList gens
      go k =
        if | k == 0 -> Iterator.upTil 1 (\_ -> List.repeat Config.maxSheets T.identity)
           | otherwise ->
             Iterator.concatMap (\m -> Iterator.map (M.sMultiply m) gens')
               (go (k - 1))
  in
  go n

isServiceableOfLength : Int -> List M.SInterp -> M.SInterp -> Bool
isServiceableOfLength n gens m =
  Iterator.all (\i ->
    Iterator.all (\m' -> not (isDiagonal (M.sMultiply m' m))) (sInterpsUpTo i gens))
    (Iterator.range 0 (n - 1))

randomTrans = rmap I.sInterpret randomIsom

timeout = 1000

-- Given gens g1 ... gk. Let H = <g_1,...,g_k>.
-- want T \in Isom(E^2) such that
-- - T = (g_i1...g_ik)^{-1} * (x, ..., x) for some x
-- - there is no w \in H with w T = (y,...,y) such that |w| < k
-- Say T = (t_1, ..., t_s)
-- There can be no eee

-- Given our random word (g_i1...g_ik) and our salt (x,...,x), w is an obstacle if
-- exists y. w (g_i1...g_ik)^{-1} = (yX,...,yX)
-- Which is the same thing as saying that w (g_i1...g_ik)^{-1} is diagonal.
-- So a word is serviceable iff there are no obstacles. (Importantly the salt
-- is irrelevant in testing serviceability.

-- (Isom(E^2))^(\oplus numSheets) / <<diagonal>>
-- <<diagonal>> = \bigcup_{x \in X} (x^G)^{\oplus numSheets}

-- Given gens g1 ... gk
-- want word (g_i1 ... g_in)^{-1} (initial state)
-- such that g_i1...gin /= g_j1..g_jm for m < n

serviceableInitialState : Int -> List M.SInterp -> Random.Generator (Maybe M.SInterp)
serviceableInitialState n gens =
  -- this is wrong. the salt can make it easy. E.g., gens are a, b.
  -- Target is aB. If salt is A, then we just have the undo B.
  -- Also, should forbid solutiont which are aaa (i.e., which are all the same trans). 
  let salted k m = rreturn (Just m)
    {-
        if | k == 0 -> rreturn Nothing
           | otherwise ->
             randomTrans `randThen` \salt -> let saltedm = M.sMultiply m (List.repeat 10 salt) in
               if allTogether saltedm then salted (k - 1) m else rreturn (Just saltedm) -}
--impossible: MLM(XL)LMS
--tooeasy:MLM(XL)
---- the problem with this is that the easy solution is not the identity, but another config
---- where both are on top each other
      invGens = List.map (List.map invert) gens
      go k =
        if k == 0
        then rreturn Nothing
        else
          randomSInterpOfLength n invGens `randThen` \mInv ->
            if isServiceableOfLength n gens mInv
            then rreturn (Just mInv)
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

      randomReflection =
        randomElem [2, 3, 4, 8]  `randThen` \denom ->
        Random.int 0 (denom - 1) `randThen` \num ->
        rreturn (I.reflection <| 2 * pi * toFloat num / toFloat denom)
  in
  Random.int 0 3 `randThen` \i -> case i of
    0 -> randomTranslation
    1 -> randomRotation
    2 -> randomReflection
    3 -> rreturn I.identity

randomMove p = Random.list p randomIsom

randomLevel difficulty =
  let (minMoves, maxMoves, minGens, maxGens, minSheets, maxSheets) = case difficulty of
        S  -> (2, 3, 2, 3, 2, 2)
        M  -> (3, 3, 2, 3, 2, 2)
        L  -> (3, 5, 3, 4, 2, 2)
        XL -> (4, 7, 3, 4, 2, 3)
  in
  Random.int minMoves maxMoves                 `randThen` \numMoves ->
  Random.int minGens maxGens                   `randThen` \numGens ->
  Random.int minSheets maxSheets               `randThen` \numSheets ->
    Random.list numGens (randomMove numSheets) `randThen` \gens ->
    let gens' = List.filter (not << List.all ((==) I.identity)) gens in
    serviceableInitialState numMoves (List.map M.sInterpret gens') `randThen` \mtrans ->
      case mtrans of
        Nothing -> randomLevel difficulty
        Just x  -> rreturn
          { availableMoves = gens'
          , initial        = x
          , maxMoves       = numMoves
          , difficulty     = difficulty
          }

