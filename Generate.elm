module Generate where

import String
import Text
import List
import List((::))
import Transform2D
import Isom as I
import Util(..)
import Move as M

wordsUpTo ident mul interp gens =
  let go n =
    if | n == 0    -> ([([], ident)], [])
       | otherwise ->
         let (ws, wss) = go (n - 1) in
         ( List.concatMap (\(w, tw) ->
             List.map (\(i,b) -> (i::w, mul (interp b) tw))
               (List.map2 (,) [0..10] gens)
           ) ws
         , ws ++ wss)
  in
  go

trans2Dclose a b = distTransform2D a b < 0.01
close xs ys = and (List.map2 trans2Dclose xs ys)

hardWords ident mul interp gens n =
  let (ws, wss) = wordsUpTo ident mul interp gens n in
  List.filterMap (\(w, tw) ->
    if List.all (\(_, tw') -> not (close tw tw')) wss
    then Just w
    else Nothing)
    ws


b1 =
  [ [I.identity, I.translation (100, 100), I.translation (-100, -100) ]
  , [I.rotation (pi/3), I.reflection (3*pi/4), I.identity]
  , [I.reflection 0, I.identity, I.rotation (-pi/3)]
  ]


b2 =
  [ [ I.reflection (2*pi/3), I.rotation (2*pi/3) ] 
  , [ I.translation (-100, 0), I.rotation (2*pi/3) ] 
  , [ I.reflection 0, I.rotation (2*pi/3) ] 
  ]

blev2 =
  [ [I.translation (-100, 0), I.translation (-100, 0)]
  , [I.translation (0, -100), I.translation (0, 100)]
  , [I.rotation (pi / 2), I.rotation (pi)]
  , [I.reflection (3*pi/4), I.identity]
  ]

main = Text.plainText <| String.join "\n" <| List.map toString <|
  hardWords (List.repeat 2 (Transform2D.rotation (pi/2)))
    M.sMultiply
    M.sInterpret
    blev2
    3

