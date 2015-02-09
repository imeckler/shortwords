module Level where

import Style(globalStyle)
import Config(..)
import Inputs(..)
import GameTypes(..)
import Window
import Util(..)
import Move
import Move(Move)
import Draw
import Graphics.Element(..)
import Graphics.Collage(..)
import Transform2D(Transform2D)
import Transform2D
import Html
import Html.Events(..)
import Html.Attributes(class)
import Text
import Color
import Time (second)
import Time
import Stage
import Stage(Stage, Forever, ForATime)
import Stage.Infix(..)
import List
import Easing (ease, float, easeInQuad, easeOutQuad)
import Signal
import Maybe

allTogether (t1::ts) =
  let closeEnough t2 = distTransform2D t1 t2 < 0.01 in
  List.all closeEnough ts

initialGameState : Game -> GameState
initialGameState (lev::levs) = 
  { levelState = initialLevelState lev
  , currLevel  = lev
  , rest       = levs
  , finished   = False
  , lastMove   = Nothing
  }

initialLevelState : Level -> LevelState
initialLevelState lev =
  { movesLeft = lev.maxMoves
  , postMove  = lev.initial
  , preMove   = lev.initial
  , endState  = Normal
  }

update : Update -> GameState -> GameState
update u s = case u of
  NextLevel      -> updateNextLevel s
  Clicked m      -> updateWithMove m s
  Hovered _      -> s
  Unhovered      -> s
  SetEndState es -> let ls = s.levelState in { s | levelState <- { ls | endState <- es } }
  NoOp           -> s

updateNextLevel s =
  case s.rest of
    []      -> { s | finished <- True }
    l :: ls ->
      { levelState = initialLevelState l
      , currLevel = l, rest = ls
      , finished = False 
      , lastMove = Nothing
      }

updateWithMove : Move -> GameState -> GameState
updateWithMove m s =
  let ls = s.levelState
      postMove' = Move.sMultiply (Move.sInterpret m) ls.postMove 
      ls' =
        case ls.endState of
          End wl Havent    -> {ls | endState <- End wl Have}
          End (Win _) Have -> ls
          _                ->
            if | allTogether postMove' ->
                let movesLeft = ls.movesLeft - 1 in
                { endState  =
                    End (Win {pre=ls.postMove, move=m, movesLeft=movesLeft})
                      Havent
                , movesLeft = movesLeft
                , postMove  = postMove'
                , preMove   = ls.postMove
                }
               | ls.movesLeft == 1      -> 
                 let s0 = (initialLevelState s.currLevel)
                     lose =
                       { pre      = ls.postMove
                       , move     = m
                       , maxMoves = s.currLevel.maxMoves
                       , init     = s.currLevel.initial
                      }
                 in
                 {s0 | endState <- End (Lose lose) Havent}
               | otherwise             ->
                 { movesLeft = ls.movesLeft - 1
                 , postMove  = postMove'
                 , preMove   = ls.postMove
                 , endState  = Normal
                 }
  in
  { s | levelState <- ls', lastMove <- Just m }


run g =
  let updates =
        Signal.mergeMany
        [ filterMap (Maybe.map Clicked) NoOp (Signal.subscribe clickMoveChan)
        , Signal.map (\_ -> NextLevel) (Signal.subscribe nextLevelChan)
        , Signal.map SetEndState (Signal.subscribe setEndStateChan)
        ]

      state         = Signal.foldp update (initialGameState g) updates

      openingScreen = let l = List.head g in Draw.plane {currTranses=l.initial, movesLeft=l.maxMoves}
      stages        = Draw.animations openingScreen updates state
      buttons       =
        Signal.map (color Color.white << Html.toElement w buttonsHeight << Draw.transButtons << .currLevel)
          state
    
      hovers = 
        Signal.subscribe hoverMoveChan
        |> Signal.dropRepeats
        |> Signal.merge (Signal.map (\_ -> Nothing)
            (Signal.subscribe clickMoveChan))
        |> Signal.map2 (\s x -> case s.levelState.endState of {
             Normal -> x; _ -> Nothing}) state

      ends_ = Draw.loseAnimEnds state

      hoverOverlay : Signal Element
      hoverOverlay =
        Signal.map2 (\mm s -> maybe empty (Draw.hoverArt s.levelState) mm)
          hovers state

      gameMode = Signal.foldp (\_ _ -> PlayLevel) TitleScreen (Signal.subscribe startGameChan)
  in
  Signal.map5 (\mode mainScreen hov butts (winW, winH) ->
    let screen =
          case mode of
            TitleScreen -> Draw.titleScreen
            PlayLevel ->
              flow down
              [ flow inward
                [ hov
                , mainScreen
                , collage w h [filled Color.white (rect w h)]
                ]
              , butts
              ]
      
        game = container winW totalHeight middle screen
    in
    flow inward
    [ globalStyle
    , container winW (4 + totalHeight) middle Draw.frame
    , game
    ])


    gameMode
    (Stage.run stages (Time.every 30))
    hoverOverlay
    buttons
    Window.dimensions

wrapWithClass c elt =
  Html.toElement (widthOf elt) (heightOf elt) (Html.div [class c] [Html.fromElement elt])

pad k e = container (widthOf e + 2*k) (heightOf e + 2*k) middle e

bordered r c elt =
  color c (container (widthOf elt + 2*r) (heightOf elt + 2*r) middle elt)

