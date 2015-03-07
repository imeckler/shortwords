module Draw where

import List ((::))
import Array
import Style(..)
import String
import Inputs(..)
import Maybe
import Config
import Move
import Move(Move)
import Html
import Html(..)
import Html.Attributes(style, class, id, href, target)
import Html.Events(..)
import GameTypes(..)
import Signal
import Color
import Piece
import Piece (Piece, ForATime, Forever)
import Piece.Infix(..)
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
import Ratio

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
  | WinE WinData
  | LoseE { pre : Move.SInterp, move : Move,init : Move.SInterp, maxMoves : Int }
  | FreshLevelE { init : Move.SInterp, maxMoves : Int }

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
        End (Win w) Havent  -> Just (WinE w)
        End (Lose l) Havent -> Just (LoseE l)
        Normal              ->
          Just (SimpleMoveE { pre=ls.preMove, move=m, movesLeft=ls.movesLeft })

    PlayLevelOfDifficulty d -> Just (FreshLevelE {init=s.currLevel.initial, maxMoves=s.currLevel.maxMoves})
    ResetLevel              -> Just (FreshLevelE {init=s.currLevel.initial, maxMoves=s.currLevel.maxMoves})
    _                       -> Nothing)
    updates state

animate : AnimatableEvent -> Piece Forever Element
animate e = case e of
  FreshLevelE d ->
    Piece.stayForever (plane {currTranses=d.init,movesLeft=d.maxMoves})
  SimpleMoveE d  -> Piece.sustain (planePiece d)
  WinE d         -> winAnim d
  LoseE d        ->
    let flashRed =
          Piece.stayFor (1/4*second) (collage w h [filled Color.red (rect w h)])
    in
    planePiece {d | movesLeft = 0} +> \p -> 
    Piece.stayFor (1/5 * second) p
    <> flashRed
    <> Piece.stayFor (1/5 * second) p
    <> flashRed 
    <> Piece.stayForever (plane {movesLeft=d.maxMoves, currTranses=d.init})
    
animations : Element -> Signal Update -> Signal GameState -> Signal (Piece Forever Element)
animations openingScreen updates state =
  filterMap (Maybe.map animate) (Piece.stayForever openingScreen)
    (animEvents updates state)

hoverArt : LevelState -> Move -> Element
hoverArt ls m =
  let rs =
    Piece.finalValue (Move.interpret m ls.postMove)
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
  -- [ filled Color.black (circle (r + 4))
  [ filled movesLeftCircleColor (circle r)
  , formText {sty | bold <- True, color <- Color.black} (toString n)
  ]
  |> move (-200, 200)

plane : AnimState -> Element
plane s =
  collage w h
  [ axes
  , group <| List.map2 (\c t -> groupTransform t [anR c]) Config.colors s.currTranses
  , movesLeftCircle s.movesLeft
  , resetButtonForm
  ]

asText' n x = Text.centered (Text.style (defTextStyle n) (Text.fromString (toString x)))
asTextWithStyle sty x = Text.centered (Text.style sty (Text.fromString (toString x)))

buttonArt : Isom -> Form
buttonArt =
  let (w, h) = (80, 80)
      r = 0.9 * min w h / 2
      thickness = 5
      thick c = let sty = solid c in {sty | width <- thickness}
      arrow a l =
        let r' = (r - 3) * min 1 ((20 + l) / Config.maxTransLen) in
        groupTransform (Transform2D.rotation a)
        [ traced (thick Color.black) (segment (-r', 0) (r', 0))
        , filled Color.black (ngon 3 10)
          |> moveX (r' - 7)
        ]
  
      fractionArt x =
        let (p, q) = Ratio.split x
            sty   = defTextStyle 22
            sty'  = {sty | color <- Color.black, bold <- True}
        in
        if | q == 1    -> toForm <| asTextWithStyle {sty' | height <- Just 30} p
           | otherwise ->
             let d     = sqrt (w^2 + h^2) / 10
                 d'    = 0.9 * d
                 sty   = let s = solid Color.black in {s | width <- 2}
                 slash = traced sty (segment (-d, -d) (d, d))
             in
             group
             [ asTextWithStyle sty' (abs p) |> toForm |> move (-d', d')
             , slash
             , asTextWithStyle sty' q |> toForm |> move (d', -d')
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
    Translation (x, y) -> arrow (atan2 y x) (sqrt (x^2 + y^2))
    Rotation a         -> group [rotArc (normalizeAngle (2 * pi * Ratio.toFloat a)), fractionArt a]
    Reflection a       -> refLine (normalizeAngle a)
    Identity           -> group []

transButtons : Level -> Html
transButtons lev = 
  let (buttonW, buttonH) = (80, 80)
      n = case lev.availableMoves of {[] -> 0; x::_ -> List.length x}
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

planePiece d =
  Piece.map (\t -> plane {currTranses=t, movesLeft=d.movesLeft})
    (Move.interpret d.move d.pre)

withOpacity o elt =
  Html.toElement (widthOf elt) (heightOf elt) <|
    div [style [("opacity", toString o)]] [Html.fromElement elt]

inRowsOfSize k = groupsOf k >> List.map (Html.div [])

diffButtonW = 100
diffButtonH = 100

difficultyButton difficulty =
  div
  [ class "win-difficulty-button"
  , onClick (Signal.send playLevelOfDifficultyChan difficulty)
  , style
    [ ("width", px diffButtonW)
    , ("height", px diffButtonH)
    ]
  ]
  [ Html.text (toString difficulty) ]

difficultyButtonsDiv =
  Html.div [] <| inRowsOfSize 2 <| List.map difficultyButton [S, M, L, XL]

chooseDifficultyScreen totalScore =
  let sty = defTextStyle 30
      sel =
        Text.fromString "Select a difficulty"
        |> Text.style sty
        |> Text.centered
        |> centeredWithWidth w
      arrow =
        group
        [ rect 90 4 |> filled Color.black
        , ngon 3 10 |> filled Color.black |> moveX 40
        ]
        |> sing |> collage 200 40 |> centeredWithWidth w
  in
  flow down [ spacer w 50, sel, arrow ]
  {-
  flow down
  [ let sty = defTextStyle 80 in
    Text.fromString "Select a difficulty"
    |> Text.style {sty | bold <- True}
    |> Text.centered
    |> centeredWithWidth w
  , Html.toElement (2 * diffButtonW) (2 * diffButtonH) <| difficultyButtonsDiv
  , Text.fromString ("Score: " ++ toString totalScore)
    |> Text.style (defTextStyle 60)
    |> Text.centered
    |> centeredWithWidth w
  ]
  -}

socialButtonSize = 40

facebookButton =
  Html.a
  [ id "facebook"
  , class "iconcon"
  , target "_blank"
  , href "https://www.facebook.com/sharer/sharer.php"
  ]
  [ div
    [ class "icon"
    , style [ ("width", px socialButtonSize), ("height", px socialButtonSize) ]
    ] []
  ]

tweetButton =
  let article difficulty = case difficulty of
        XL -> "an"
        _  -> "a"
  in
  \difficulty score ->
  Html.a
  [ id "tweet"
  , class "iconcon"
  , target "_blank"
  , href
    <| "https://twitter.com/intent/tweet?text=I+just+completed+"
       ++ article difficulty ++ toString difficulty
       ++ "+puzzle+in+%23ShortWords+and+my+score+is+"
       ++ toString score
       ++ ".+http%3A%2F%2Fbit.ly%2F1BWGPNt"
  ]
  [ div
    [ class "icon"
    , style
      [ ("width", px socialButtonSize), ("height", px socialButtonSize) ]
    ]
    []
  ]

winAnim d =
  let fadeTime    = 1 * second
      winScreen t =
        div
        [ id "win-screen", style [("opacity", toString (t / fadeTime))] ]
        [ div [id "win-screen-inner"]
          [ div []
            [ h1 [] [ text "You win!" ]
            , span
              [ class "score" ]
              [ text <| "Score: " ++ toString d.totalScore ]
            ]
          , div []
            [ facebookButton, tweetButton d.difficulty d.totalScore ]
          ]
        ]
        |> Html.toElement w h
        {-
        flow down
        [ let sty = defTextStyle 80 in
          Text.fromString "You win!"
          |> Text.style {sty | bold <- True}
          |> Text.centered
          |> centeredWithWidth w
        , Text.fromString ("Score: " ++ toString d.totalScore)
          |> Text.style (defTextStyle 50)
          |> Text.centered
          |> centeredWithWidth w
        , tweetButton d.difficulty d.totalScore
--        , Html.toElement (2 * diffButtonW) (2 * diffButtonH) <| difficultyButtonsDiv
        ]
        |> container w h middle
        |> color fadeColor
        |> withOpacity (t / fadeTime)
        -}
  in
  (planePiece d
  +> \p ->
  Piece.stayFor (1/2*second) p
  <> Piece.for fadeTime (\t -> 
    flow inward
    [ winScreen t, p]))
  |> Piece.sustain

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

difficultyButtonsOverlay =
  List.map overlayDifficultyButton [S, M, L, XL]
  |> div [id "level-button-panel"]
  |> Html.toElement levelButtonW (4 * levelButtonH)

overlayDifficultyButton d =
  div
  [ class "level-button active", onClick (Signal.send playLevelOfDifficultyChan d) ]
  [ Html.text (toString d) ]

resetButtonForm = toForm resetButton |> move (w/2 - 60, -h/2 + 40)

