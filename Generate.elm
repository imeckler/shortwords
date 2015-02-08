module Generate where

import List
import List((::))
import Transform2D
import Isom as I
import Util(..)

wordsUpTo basis n =
  if | n == 0    -> ([([], Transform2D.identity)], [])
     | otherwise ->
       let (ws, wss) = wordsUpTo basis (n - 1) in
       ( List.concatMap (\(w, tw) ->
           List.map (\b -> (b::w, Transform2D.multiply (I.sInterpret b) tw))
             basis
         ) ws
       , ws ++ wss)

close a b = distTransform2D a b < 0.01

hardWords basis n =
  let (ws, wss) = wordsUpTo basis n in
  List.filterMap (\(w, tw) ->
    if List.all (\(_, tw') -> not (close tw tw')) wss
    then Just w
    else Nothing)
    ws

