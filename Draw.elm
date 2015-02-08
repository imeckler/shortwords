module Draw where

import String
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
  | GameWonE { pre : Move.SInterp, move : Move, movesLeft : Int }
  | LoseE { pre : Move.SInterp, move : Move,init : Move.SInterp, maxMoves : Int }
  | NextLevelE { init : Move.SInterp, maxMoves : Int }

onLastLevel = List.isEmpty << .rest

animEvents : Signal Update -> Signal GameState -> Signal (Maybe AnimatableEvent)
animEvents updates state =
  Signal.map2 (\u s -> let ls = s.levelState in case u of
    Clicked m -> case ls.endState of
      End _ Have          -> Nothing
      End (Win w) Havent  -> Just (if onLastLevel s then GameWonE w else WinE w)
      End (Lose l) Havent -> Just (LoseE l)
      Normal              ->
        Just (SimpleMoveE { pre=ls.preMove, move=m, movesLeft=ls.movesLeft })

    NextLevel -> Just (NextLevelE {init=s.currLevel.initial, maxMoves=s.currLevel.maxMoves})
    _          -> Nothing)
    updates state

animate : AnimatableEvent -> Stage Forever Element
animate e = case e of
  GameWonE d ->
    (planeStage d
    +> \p -> Stage.for (1*second) (\t ->
      flow outward
      [ p
      , Text.fromString "The end" |> Text.style (defTextStyle 60) |> Text.centered
        |> container w h middle |> color fadeColor
        |> withOpacity (t / (1*second))
      ]))
    |> Stage.sustain

  NextLevelE d ->
    Stage.stayForever (plane {currTranses=d.init,movesLeft=d.maxMoves})
  SimpleMoveE d  -> Stage.sustain (planeStage d)
  WinE d         -> winAnim d
  LoseE d        ->
    let flashRed =
          Stage.stayFor (1/4*second) (collage w h [filled Color.red (rect w h)])
    in
    planeStage {d | movesLeft = 0} +> \p -> 
    Stage.stayFor (1/5 * second) p
    <> flashRed
    <> Stage.stayFor (1/5 * second) p
    <> flashRed 
    <> Stage.stayForever (plane {movesLeft=d.maxMoves, currTranses=d.init})
    
animations : Element -> Signal Update -> Signal GameState -> Signal (Stage Forever Element)
animations openingScreen updates state =
  filterMap (Maybe.map animate) (Stage.stayForever openingScreen)
    (animEvents updates state)

lighten c =
  let {hue,saturation,lightness,alpha} = Color.toHsl c in
  Color.hsla hue saturation (2*lightness) 1

hoverArt : LevelState -> Move -> Element
hoverArt ls m =
  Stage.finalValue (Move.interpret m ls.postMove)
  |> List.map2 (\c t -> groupTransform t [anR (withAlpha 0.5 <| lighten c)])
      Config.colors
  |> collage w h

titleScreen : Element
titleScreen =
  flow down
  [ Text.fromString "Short Words"
    |> Text.style (defTextStyle 80)
    |> Text.centered
    |> centeredWithWidth w
  , Html.div
    [ onClick (Signal.send startGameChan ())
    , class "swbutton"
    , customButtonStyle
    ]
    [ Html.text "Play" ]
    |> Html.toElement w 50
  ]
  |> container w totalHeight middle |> color fadeColor


plane : AnimState -> Element
plane s =
  let movesLeft = let r = 30 in
        group
        [ filled Color.black (circle (r + 4))
        , filled movesLeftCircleColor (circle r)
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
        let r' = r - 3 in
        groupTransform (Transform2D.rotation a)
        [ traced (thick Color.black) (segment (-r', 0) (r', 0))
        , filled Color.black (ngon 3 10)
          |> moveX r'
        ]
  
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
          |> rotate ((-pi /6) + if a < 0 then pi else 0)
          |> moveX r'
          |> sing |> groupTransform (Transform2D.rotation a) -- why doesn't rotate work...
        ]

      refLine a =
        group
        [ traced (thick Color.green) (segment (-r, 0) (r, 0))
        ] |> rotate a
  in
  \t -> case t of
    Translation (x, y) -> arrow (atan2 y x)
    Rotation a       -> rotArc (normalizeAngle a)
    Reflection a     -> refLine (normalizeAngle a)
    Identity         -> group []

transButtons : Level -> Html
transButtons lev = 
  let (buttonW, buttonH) = (80, 80)
      n = List.length <| List.head <| lev.availableMoves
      moveButton m =
        List.map3 (\i c t ->
          div
          [ style <|
            [ ("height", px buttonH), ("width", px buttonW)
            , ("backgroundColor", colorStr c)
            ]
          , class <|
              if | i == 0 -> "top-isom isom"
                 | i == n - 1 -> "bottom-isom isom"
                 | otherwise -> "isom"
          ]
          [ fromElement (collage buttonW buttonH [buttonArt t]) ]
        )
        [0..(n - 1)] Config.colors m
        |> div
        [ onClick (Signal.send clickMoveChan (Just m))
        , onMouseEnter (Signal.send hoverMoveChan (Just m))
        , onMouseLeave (Signal.send hoverMoveChan Nothing)
        , class "movebutton"
        , style
          [ ("display", "inline-block")
          , ("padding", "5px")
          , ("pointerEvents", "auto")
          ]
        ]
  in
  div
  [ style
    [ ("textAlign", "center")
    ]
  ] 
  <| List.map moveButton <| lev.availableMoves

planeStage d =
  Stage.map (\t -> plane {currTranses=t, movesLeft=d.movesLeft})
    (Move.interpret d.move d.pre)

-- winAnim : Move -> GameState -> Stage Forever Element

withOpacity o elt =
  Html.toElement (widthOf elt) (heightOf elt) <|
    div [style [("opacity", toString o)]] [Html.fromElement elt]

winAnim d =
  let fadeTime               = 1 * second
      winScreen t withButton =
        flow down (
        [ Text.fromString "You win!"
          |> Text.style (defTextStyle 80)
          |> Text.centered
          |> centeredWithWidth w
        ]
        ++ if withButton then
        [ Html.div
          [ onClick (Signal.send nextLevelChan ())
          , class "swbutton"
          ]
          [ Html.text "Next Level" ]
          |> Html.toElement w 30
        ] else [])
        |> container w h middle
        |> color fadeColor
        |> withOpacity (t / fadeTime)

  in
  (planeStage d
  +> \p ->
  Stage.stayFor (1/2*second) p
  <> Stage.for fadeTime (\t -> 
    flow inward
    [ winScreen t True, p]))
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

-- colors
backgroundColor       = Color.rgb 223 223 223
borderColor           = Color.rgb 188 188 188
fadeColor             = Color.rgb 254 204 9
winTextColor          = Color.rgb 75 91 110
buttonBackgroundColor = winTextColor
rotateArcColor        = fadeColor
movesLeftTextColor    = winTextColor
movesLeftCircleColor  = fadeColor
defTextStyle h        =
  { typeface = ["Futura", "sans-serif"]
  , height   = Just h
  , color    = winTextColor
  , bold     = False
  , italic   = False
  , line     = Nothing
  }
defaultFontStr = String.join "," ((defTextStyle 0).typeface)

customButtonStyle =
  style
  [ ("pointerEvents", "auto")
  ]

globalStyle = 
  let buttonColor = Color.rgb 0 119 219 in
  Html.toElement 0 0 <| styleNode <|
  [ ( ".swbutton"
    , [ ("pointer-events", "auto")
      , ("width", px customButtonW)
      , ("height", px customButtonH)
      , ("line-height", px customButtonH)
      , ("background-color", colorStr buttonColor)
      , ("text-align", "center")
      , ("border-radius", px 9)
      , ("border", "3px solid black")
      , ("margin", "0 auto")
      , ("font-family", defaultFontStr)
      , ("font-size", px 20)
      , ("color", colorStr Color.white)
      , ("cursor", "pointer")
      ]
    )
  , ( ".swbutton:hover"
    , [ ("background-color", colorStr (lighten buttonColor)) ]
    )
  , ( ".top-isom"
    , [ ("border-top-left-radius", px 8)
      , ("border-top-right-radius", px 8)
      , ("border-top", "4px solid black")
      ]
    )
  , ( ".bottom-isom"
    , [ ("border-bottom-left-radius", px 8)
      , ("border-bottom-right-radius", px 8)
      , ("border-bottom", "4px solid black")
      ]
    )
  , ( ".isom"
    , [ ("border-left", "4px solid black")
      , ("border-right", "4px solid black")
      , ("cursor", "pointer")
      ]
    )
  , ( ".movebutton:hover"
    , [("opacity", toString 0.6)]
    )
  ]

customButtonW = 100
customButtonH = 50

