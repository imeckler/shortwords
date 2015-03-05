module Util where

--debug
import Text
import Debug

import Array
import Color
import Graphics.Element(..)
import String
import Html
import List
import List((::))
import Native.IsomUtil
import Transform2D
import Signal
import Signal(Signal, (~))
import Ratio

sing x = [x]

filterFold : (a -> b -> Maybe b) -> b -> Signal a -> Signal b
filterFold f z s =
  Signal.foldp (\a (b,keep) -> case f a b of
    Just b' -> (b', True)
    Nothing -> (b, False))
    (z, True) s
  |> Signal.keepIf snd (z, True)
  |> Signal.map fst

filterMap : (a -> Maybe b) -> b -> Signal a -> Signal b
filterMap f y = filterFold (\x _ -> f x) y

filterJust : a -> Signal (Maybe a) -> Signal a
filterJust = filterMap identity

firstDo x y = Transform2D.multiply y x

maybe : b -> (a -> b) -> Maybe a -> b
maybe y f mx = case mx of
  Nothing -> y
  Just x -> f x

and xs = case xs of
  x :: xs' -> x && and xs'
  []       -> True

modFloat : Float -> Float -> Float
modFloat x m = x - m * toFloat (floor (x / m))

-- angles are in [-pi, pi) for convenience of
-- going to short way around
normalizeAngle x = 
  let x' = modFloat x (2 * pi) in if x' > pi then x' - 2 * pi else x'

normalizeCirculan r =
  let (a, b) = Ratio.split r
      a' = a % b
      a'' = if a' >= (b // 2 + (b % 2)) then a' - b else a'
  in
  a'' `Ratio.over` b

tuply : Transform2D.Transform2D -> (Float, Float, Float, Float, Float, Float)
tuply = Native.IsomUtil.tuply

invert : Transform2D.Transform2D -> Transform2D.Transform2D
invert m =
  let (a,b,x,c,d,y) = tuply m
      det = a * d - b * c
  in
  if det == 0
  then Debug.crash ("0 determinant: " ++ toString (tuply m))
  else
    let s = 1 / det in
    Transform2D.matrix (s*d) (-s*b) (-s*c) (s*a) (-x) (-y)

distTransform2D trans goal =
    let (g0,g1,g2,g3,g4,g5) = tuply goal
        (t0,t1,t2,t3,t4,t5) = tuply trans
    in
    List.map2 (\g t -> (g - t)^2)
      [g0,g1,g2,g3,g4,g5] [t0,t1,t2,t3,t4,t5]
    |> List.sum

styleNode stys =
  List.map (\(sel, rs) ->
    sel ++ " " ++ "{" ++
      String.join "\n" (List.map (\(k, v) -> k ++ ":" ++ v ++ ";") rs)
    ++ "}") stys
  |> String.join "\n"
  |> (Html.node "style" [] << sing << Html.text)

colorStr c =
  let {red,green,blue,alpha} = Color.toRgb c in
  "rgba(" ++
  toString red ++ "," ++ 
  toString green ++ "," ++
  toString blue ++ "," ++
  toString alpha ++ ")"

px n = toString n ++ "px"

onLastLevel s = Array.length s.levels == s.currLevelIndex

listInit f n =
  let go i = if i == n then [] else f i :: go (i + 1) in
  go 0

signalMap6 f s1 s2 s3 s4 s5 s6 =
  Signal.map5 f s1 s2 s3 s4 s5 ~ s6

splitAt k xs =
  if | k == 0    -> ([], xs)
     | otherwise -> case xs of
       []     -> ([], xs)
       x::xs' -> let (a, b) = splitAt (k - 1) xs' in (x::a, b)

groupsOf k xs =
  let (a, b) = splitAt k xs in
  case b of
    [] -> [a]
    _  -> a :: groupsOf k b

allTogether (t1::ts) =
  let closeEnough t2 = distTransform2D t1 t2 < 0.01 in
  List.all closeEnough ts

