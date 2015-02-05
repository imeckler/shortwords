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

filterMap2 : (a -> b -> Maybe c) -> Signal a -> Signal b -> Signal (Maybe c)
filterMap2 f a b =  <| Signal.map2 f a b

filterJust : a -> Signal (Maybe a) -> Signal a
filterJust =
  let isJust x = case x of {Just _ -> True; _ -> False}
  in keepIf isJust

firstDo x y = Transform2D.multiply y x

