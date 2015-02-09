module Draw where

import List ((::))
import Array
import Style(..)
import Debug
import String
import Inputs(..)
import Maybe
import Config
import Move
import Move(Move)
import Html
import Html(..)
import Html.Attributes(style, class, id)
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
import Time
import Time(second)
import Easing (ease, float, easeInQuad, easeOutQuad)
import Config(..)
import Text
import List
import Transform2D
import Isom as I
import Isom(Isom(..))
import Native.Execute

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

anR color =
  let sty = defTextStyle 55 in
  Text.style {sty | bold <- True, color <- color} (Text.fromString "R")
  |> Text.centered
  |> toForm

type AnimatableEvent
  = SimpleMoveE { pre : Move.SInterp, move : Move, movesLeft : Int }
  | WinE { pre : Move.SInterp, move : Move, movesLeft : Int }
  | GameWonE { pre : Move.SInterp, move : Move, movesLeft : Int }
  | LoseE { pre : Move.SInterp, move : Move,init : Move.SInterp, maxMoves : Int }
  | FreshLevelE { init : Move.SInterp, maxMoves : Int }

onLastLevel s = Array.length s.levels == s.currLevelIndex

loseAnimEnds : Signal GameState -> Signal ()
loseAnimEnds state =
  filterMap (\s -> case s.levelState.endState of
    End (Lose _) Havent -> Just (Signal.send setEndStateChan Normal)
    _                   -> Nothing)
    (Signal.send setEndStateChan Normal) state
  |> Time.delay loseAnimDuration
  |> Native.Execute.schedule

animEvents : Signal Update -> Signal GameState -> Signal (Maybe AnimatableEvent)
animEvents updates state =
  Signal.map2 (\u s -> let ls = s.levelState in case u of
    Clicked m ->
      case ls.endState of
        End _ Have          -> Nothing
        End (Win w) Havent  -> Just (if onLastLevel s then GameWonE w else WinE w)
        End (Lose l) Havent -> Debug.log "hi" <| Just (LoseE l)
        Normal              ->
          Just (SimpleMoveE { pre=ls.preMove, move=m, movesLeft=ls.movesLeft })

    NextLevel  -> Just (FreshLevelE {init=s.currLevel.initial, maxMoves=s.currLevel.maxMoves})
    ResetLevel -> Just (FreshLevelE {init=s.currLevel.initial, maxMoves=s.currLevel.maxMoves})
    SetLevel _ -> Just (FreshLevelE {init=s.currLevel.initial, maxMoves=s.currLevel.maxMoves})
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

  FreshLevelE d ->
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

hoverArt : LevelState -> Move -> Element
hoverArt ls m =
  let rs =
    Stage.finalValue (Move.interpret m ls.postMove)
    |> List.map2 (\c t -> groupTransform t [anR (withAlpha 0.5 <| lighten c)])
        Config.colors
    |> group
  in
  collage w h [ axes, movesLeftCircle ls.movesLeft, rs, resetButtonForm ]
  |> color Color.white

centeredWithWidth w e =
  container w (heightOf e) middle e

titleScreen : Element
titleScreen =
  Html.div [ style [("marginTop", "-40px")] ]
  [ Html.div [id "titletext"]
    [ Html.text "Short Words" ]
  , Html.p [ id "explanationtext" ]
    [ Html.text "The goal of the game is to get all R's in the same position using the moves available. The icons on the buttons indicate what effect they have."
    ]
  , Html.div
    [ onClick (Signal.send startGameChan ())
    , class "swbutton"
    ]
    [ Html.text "Play" ]
  ]
  |> Html.toElement w 200
  |> container w totalHeight middle |> color fadeColor

axes =
  let mk (a, b) = traced (dotted Color.black) (segment (-a, -b) (a, b)) in
  group [mk (w/2, 0), mk (0, h/2)]

movesLeftCircle n =
  let r = 30
      sty = defTextStyle 40
  in
  group
  [ filled Color.black (circle (r + 4))
  , filled movesLeftCircleColor (circle r)
  , formText {sty | bold <- True, color <- Color.black} (toString n)
  ]
  |> move (-200, 200)

plane : AnimState -> Element
plane s =
  collage w h
  [ axes
  , movesLeftCircle s.movesLeft
  , group <| List.map2 (\c t -> groupTransform t [anR c]) Config.colors s.currTranses
  , resetButtonForm
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
          |> moveX (r' - 7)
        ]
  
      arc r a =
        let n = 50
            t = a / n
            f i = (r * cos (t*i), r * sin (t*i))
        in List.map f [0..n-1]
  
      rotArc a =
        let r' = r - 3 in
        let arr =
              group
              [ traced (thick rotateArcColor) (arc r' a)
              , ngon 3 10
                |> filled rotateArcColor
                |> rotate ((-pi /6) + if a < 0 then pi else 0)
                |> moveX r'
                |> sing |> groupTransform (Transform2D.rotation a) -- why doesn't rotate work...
              ]
        in
        if abs a < pi then group [arr, groupTransform (Transform2D.rotation pi) [arr]]
                      else arr

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
        [ let sty = defTextStyle 80 in
          Text.fromString "You win!"
          |> Text.style {sty | bold <- True}
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

frame =
  div [ id "mainframe" ] [] |> Html.toElement frameWidth frameHeight

loseAnimDuration = transitionTime + second * (1/5 + 1/4 + 1/5 + 1/4)

resetButton =
  div
  [ class "swbutton"
  , onClick (Signal.send resetLevelChan ())
  , style  [ ("width", px 100) ]
  ]
  [ Html.text "Reset" ]
  |> Html.toElement w customButtonH

levelButtons s =
  let n = Array.length s.levels in
  listInit (\i -> levelButton i (i <= s.highestLevel)) n
  |> div [id "level-button-panel"]
  |> Html.toElement levelButtonW (n * levelButtonH)

levelButton i active =
  div
  (  class ("level-button " ++ if active then "active" else "inactive")
  :: if active then [onClick (Signal.send setLevelChan i)] else [])
  [ Html.text (toString i) ]

resetButtonForm = toForm resetButton |> move (w/2 - 60, -h/2 + 40)
