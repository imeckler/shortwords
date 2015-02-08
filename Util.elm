module Util where

import Graphics.Element(..)
import String
import Html
import List
import List((::))
import Native.IsomUtil
import Transform2D
import Signal

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
  let x' = modFloat x (2 * pi) in if x' >= pi then x' - 2 * pi else x'

tuply : Transform2D.Transform2D -> (Float, Float, Float, Float, Float, Float)
tuply = Native.IsomUtil.tuply

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

centeredWithWidth w e =
  container w (heightOf e) middle e

