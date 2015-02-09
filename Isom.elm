module Isom where

import Util(..)
import Transform2D
import Transform2D(Transform2D)
import List
import Stage
import Stage(Stage, ForATime)
import Easing(..)
import Config(transitionTime)

type Isom
  = Translation (Float, Float)
  | Rotation Float
  | Reflection Float -- angle in radians
  | Identity

sInterpret : Isom -> Transform2D
sInterpret m = case m of
  Identity -> Transform2D.identity
  Translation pt -> uncurry Transform2D.translation pt
  Rotation a -> Transform2D.rotation a
  Reflection a ->
    List.foldr1 Transform2D.multiply
    [ Transform2D.rotation a
    , Transform2D.scaleY -1
    , Transform2D.rotation (-a)
    ]

interpret : Isom -> (Transform2D -> Stage ForATime Transform2D)
interpret t tInit = Stage.map (firstDo tInit) <| Stage.for transitionTime <| case t of
  Identity -> (\_ -> Transform2D.identity)

  Translation pt ->
    uncurry Transform2D.translation
    << ease easeInOutQuad (pair float) (0,0) pt transitionTime

  Rotation x ->
    Transform2D.rotation
    << ease easeInOutQuad float 0 x transitionTime
  
  Reflection a ->
    let r    = Transform2D.rotation a
        rInv = Transform2D.rotation -a
    in
    (\x -> Transform2D.multiply r (Transform2D.multiply x rInv))
    << Transform2D.scaleY
    << ease easeInOutQuad float 1 -1 transitionTime

firstDo x y = Transform2D.multiply y x

rotation = Rotation << normalizeAngle
translation = Translation
reflection = Reflection << normalizeAngle 
identity = Identity
