module Isom where

import Util exposing (..)
import Transform2D
import Transform2D exposing (Transform2D)
import List
import Piece
import Piece exposing (Piece, ForATime)
import Easing exposing (..)
import Config exposing (transitionTime)
import Ratio

type Isom
  = Translation (Float, Float)
  | Rotation Ratio.Rational -- in circulans
  | Reflection Float -- angle in radians
  | Identity

rationalRot x = 2 * pi * Ratio.toFloat x

sInterpret : Isom -> Transform2D
sInterpret m = case m of
  Identity       -> Transform2D.identity
  Translation pt -> uncurry Transform2D.translation pt
  Rotation r     -> Transform2D.rotation (rationalRot r)
  Reflection a   ->
    foldr1 Transform2D.multiply
    [ Transform2D.rotation a
    , Transform2D.scaleY -1
    , Transform2D.rotation (-a)
    ]

interpret : Isom -> (Transform2D -> Piece ForATime Transform2D)
interpret t tInit = Piece.map (firstDo tInit) <| Piece.for transitionTime <| case t of
  Identity -> (\_ -> Transform2D.identity)

  Translation pt ->
    uncurry Transform2D.translation
    << ease easeInOutQuad (pair float) (0,0) pt transitionTime

  Rotation r ->
    Transform2D.rotation
    << ease easeInOutQuad float 0 (rationalRot r) transitionTime

  Reflection a ->
    let r    = Transform2D.rotation a
        rInv = Transform2D.rotation -a
    in
    (\x -> Transform2D.multiply r (Transform2D.multiply x rInv))
    << Transform2D.scaleY
    << ease easeInOutQuad float 1 -1 transitionTime

firstDo x y = Transform2D.multiply y x

rotation = Rotation << normalizeCirculan
translation = Translation
reflection = Reflection << normalizeAngle 
identity = Identity
