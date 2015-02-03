module Move where

import Transform2D
import Transform2D(Transform2D)
import List
import Stage
import Stage(Stage, ForATime)
import Time(second)
import Easing(..)

type Move
  = Translate (Float, Float)
  | Rotation Float
  | Reflection Float -- angle in radians

asTransform : Move -> Transform2D
asTransform m = case m of
  Translate pt -> uncurry Transform2D.translation pt
  Rotation a -> Transform2D.rotation a
  Reflection a ->
    List.foldr1 Transform2D.multiply
    [ Transform2D.rotation a
    , Transform2D.scaleY -1
    , Transform2D.rotation (-a)
    ]

interpret : Move -> (Transform2D -> Stage ForATime Transform2D)
interpret t tInit = Stage.map (firstDo tInit) <| Stage.for second <| case t of
  Translate pt ->
    uncurry Transform2D.translation
    << ease easeInOutQuad (pair float) (0,0) pt second

  Rotation x ->
    Transform2D.rotation
    << ease easeInOutQuad float 0 x second
  
  Reflection a ->
    let r    = Transform2D.rotation a
        rInv = Transform2D.rotation -a
    in
    (\x -> Transform2D.multiply r (Transform2D.multiply x rInv))
    << Transform2D.scaleY
    << ease easeInOutQuad float 1 -1 second

firstDo x y = Transform2D.multiply y x
