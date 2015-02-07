module Draw where

import Inputs(..)
import Maybe
import Config
import Move
import Move(Move)
import Html
import Html(..)
import Html.Attributes(style, class)
import Html.Events(..)
import GameTypes(..)
import Signal
import Color
import Stage
import Stage (Stage, ForATime, Forever)
import Stage.Infix(..)
import Util(..)
import Graphics.Collage(..)
import Graphics.Element(..)
import Time(second)
import Easing (ease, float, easeInQuad, easeOutQuad)
import Config(..)
import Text
import List
import Transform2D
import Isom as I
import Isom(Isom(..))

withAlpha a c = let {red,green,blue} = Color.toRgb c in
  Color.rgba red green blue a

-- would prefer this not be firing once done... that's really a shame.
-- especially since one doesn't think of this as events. Consider making
-- fancier type that holds onto constant forever value if there is one
loseOverlay =
  let dur = (1/8) * second
      alpha = Stage.stayFor (second / 4) 1
--        Stage.for dur (ease easeInQuad float 0 1 dur)
--        <> Stage.for dur (ease easeOutQuad float 1 0 dur)
      screen =
        Stage.map (\a -> filled (withAlpha a Color.red) (rect w h)) alpha
  in
  filterMap (\s ->
    if s.justLost then Just (screen <> Stage.stayForever (group [])) else Nothing)
    (Stage.stayForever (group []))

withBorder b c e =
  color c
    (container (widthOf e + (2 * b)) (heightOf e + (2 * b)) middle e)

formText sty =
  Text.fromString
  >> Text.style sty 
  >> Text.centered
  >> toForm

defaultStyle =
  { height   = Just 14
  , color    = Color.black
  , typeface = ["Futura", "sans-serif"]
  , bold     = True
  , italic   = False
  , line     = Nothing
  }

anR color =
  Text.style {defaultStyle | height <- Just 50, color <- color} (Text.fromString "R")
  |> Text.centered
  |> toForm

type AnimatableEvent
  = SimpleMoveE { pre : Move.SInterp, move : Move, movesLeft : Int }
  | WinE { pre : Move.SInterp, move : Move, movesLeft : Int }
  | LoseE { pre : Move.SInterp, move : Move,init : Move.SInterp, maxMoves : Int }
  | NextLevelE { init : Move.SInterp, maxMoves : Int }

animEvents : Signal Update -> Signal GameState -> Signal (Maybe AnimatableEvent)
animEvents updates state =
  Signal.map2 (\u s -> let ls = s.levelState in case u of
    Clicked m -> case ls.endState of
      End _ Have          -> Nothing
      End (Win w) Havent  -> Just (WinE w)
      End (Lose l) Havent -> Just (LoseE l)
      Normal              ->
        Just (SimpleMoveE { pre=ls.preMove, move=m, movesLeft=ls.movesLeft })

    NextLevel -> Just (NextLevelE {init=s.currLevel.initial, maxMoves=s.currLevel.maxMoves})
    _          -> Nothing)
    updates state

animate : AnimatableEvent -> Stage Forever Element
animate e = case e of
  NextLevelE d ->
    Stage.stayForever (plane {currTranses=d.init,movesLeft=d.maxMoves})
  SimpleMoveE d  -> Stage.sustain (planeStage d)
  WinE d         -> winAnim d
  LoseE d        ->
    let flashRed =
          Stage.stayFor (1/2*second) (collage w h [filled Color.red (rect w h)])
    in
    planeStage {d | movesLeft = d.maxMoves} +> \p -> 
    flashRed
    <> Stage.stayFor (1/2 * second) p
    <> flashRed 
    <> Stage.stayForever (plane {movesLeft=d.maxMoves, currTranses=d.init})
    
animations : Element -> Signal Update -> Signal GameState -> Signal (Stage Forever Element)
animations openingScreen updates state =
  filterMap (Maybe.map animate) (Stage.stayForever openingScreen)
    (animEvents updates state)

plane : AnimState -> Element
plane s =
  let movesLeft =
        group
        [ filled movesLeftCircleColor (circle 30)
        , formText {defaultStyle | height <- Just 30} (toString s.movesLeft)
        ]
        |> move (-200, 200)

      axes =
        let mk (a, b) = traced (dotted Color.black) (segment (-a, -b) (a, b)) in
        group [mk (w/2, 0), mk (0, h/2)]
  in
  collage w h
  [ axes
  , movesLeft
  , group <| List.map2 (\c t -> groupTransform t [anR c]) Config.colors s.currTranses
  ]

buttonArt : Isom -> Form
buttonArt =
  let (w, h) = (80, 80)
      r = 0.9 * min w h / 2
      thickness = 5
      thick c = let sty = solid c in {sty | width <- thickness}
      arrow a =
        group
        [ traced (thick Color.black) (segment (0, -r) (0, r))
        , filled Color.black (ngon 3 5)
        ] |> rotate a
  
      arc r a =
        let n = 50
            t = a / n
            f i = (r * cos (t*i), r * sin (t*i))
        in List.map f [0..n-1]
  
      rotArc a =
        let r' = r - 3 in
        group
        [ traced (thick rotateArcColor) (arc r' a)
        , ngon 3 10
          |> filled rotateArcColor
          |> rotate (-pi /6)
          |> moveX r'
          |> sing |> groupTransform (Transform2D.rotation a) -- why doesn't rotate work...
        ]

      refLine a =
        group
        [ traced (thick Color.green) (segment (-r, 0) (r, 0))
        ] |> rotate a
  in
  \t -> case t of
    Translate (x, y) -> arrow (atan2 y x)
    Rotation a       -> rotArc a
    Reflection a     -> refLine a
    Identity         -> group []

transButtons : Level -> Html
transButtons = 
  let (buttonW, buttonH) = (80, 80)
      moveButton m =
        List.map2 (\c t ->
          div
          [ style
            [ ("height", px buttonH), ("width", px buttonW)
            , ("backgroundColor", colorStr c)
            ]
          ]
          [ fromElement (collage buttonW buttonH [buttonArt t]) ]
        )
        Config.colors m
        |> div
        [ onClick (Signal.send clickMoveChan (Just m))
        , onMouseEnter (Signal.send hoverMoveChan (Just m))
        , onMouseLeave (Signal.send hoverMoveChan Nothing)
        , class "movebutton"
        , style
          [ ("display", "inline-block")
          , ("padding", "5px")
          , ("pointEvents", "auto")
          ]
        ]
  in
  div
  [ style
    [ ("textAlign", "center")
    ]
  ] 
  << List.map moveButton << .availableMoves

planeStage d =
  Stage.map (\t -> plane {currTranses=t, movesLeft=d.movesLeft})
    (Move.interpret d.move d.pre)

-- winAnim : Move -> GameState -> Stage Forever Element
winAnim d =
  let fadeTime               = 1 * second
      winScreen t withButton =
        div
        [ style
          [ ("backgroundColor", colorStr fadeColor)
          , ("width", px w)
          , ("height", px h)
          , ("opacity", toString (t / fadeTime))
          , ("text-align", "center")
          , ("pointEvents", "auto")
          ]
        ]
        ([ Html.p [] [Html.text "You\n\nwin!"] ]
        ++ if withButton
           then
           [ button 
             [ onClick (Signal.send nextLevelChan ()) ]
             [ Html.text "Next" ]
           ]
           else [])

  in
  (planeStage d
  +> \p -> Stage.for fadeTime (\t -> 
    flow inward
    [ Html.toElement w h (winScreen t True), p]))
  |> Stage.sustain

withButtons mainScreen s =
  flow down
  [ mainScreen
  , Html.toElement w 300 (transButtons s.currLevel)
  ]

-- UTIL
colorStr c =
  let {red,green,blue,alpha} = Color.toRgb c in
  "rgba(" ++
  toString red ++ "," ++ 
  toString green ++ "," ++
  toString blue ++ "," ++
  toString alpha ++ ")"

px n = toString n ++ "px"
sing x = [x]

-- colors
backgroundColor       = Color.rgb 223 223 223
borderColor           = Color.rgb 188 188 188
fadeColor             = Color.rgb 254 204 9
winTextColor          = Color.rgb 75 91 110
buttonBackgroundColor = winTextColor
rotateArcColor        = fadeColor
movesLeftTextColor    = winTextColor
movesLeftCircleColor  = fadeColor
