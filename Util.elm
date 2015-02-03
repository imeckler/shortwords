module Util where

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

