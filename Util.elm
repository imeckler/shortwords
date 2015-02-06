module Util where

import Transform2D
import Signal

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

