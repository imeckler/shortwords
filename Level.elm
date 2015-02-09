module Level where

import Array
import Native.Execute
import Debug
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
initialGameState levs = let lev = levs ! 0 in
  { levelState     = initialLevelState lev
  , currLevel      = lev
  , currLevelIndex = 0
  , levels         = levs
  , finished       = False
  , lastMove       = Nothing
  , highestLevel   = 0
  }

initialLevelState : Level -> LevelState
initialLevelState lev =
  { movesLeft = lev.maxMoves
  , postMove  = lev.initial
  , preMove   = lev.initial
  , endState  = Normal
  }

setLevel : Int -> GameState -> GameState
setLevel i s = let lev = s.levels ! i in
  {s | currLevelIndex <- i, currLevel <- lev, levelState <- initialLevelState lev}

update : Update -> GameState -> GameState
update u s = case u of
  NextLevel         -> updateNextLevel s
  Clicked m         -> updateWithMove m s
  Hovered _         -> s
  Unhovered         -> s
  SetLevel i        -> setLevel i s
  SetHighestLevel n -> {s | highestLevel <- n}
  SetEndState es    -> let ls = s.levelState in { s | levelState <- { ls | endState <- es } }
  ResetLevel        -> {s | levelState <- initialLevelState s.currLevel}
  NoOp              -> s

(!) a i = case Array.get i a of Just x -> x

updateNextLevel s =
  if onLastLevel s
  then {s | finished <- True}
  else
    let i         = s.currLevelIndex + 1
        currLevel = s.levels ! i
    in
    { levelState     = initialLevelState currLevel
    , highestLevel   = max s.highestLevel i
    , currLevelIndex = i
    , currLevel      = currLevel
    , levels         = s.levels
    , finished       = False
    , lastMove       = Nothing
    } 

updateWithMove : Move -> GameState -> GameState
updateWithMove m s =
  let ls = s.levelState
      postMove' = Move.sMultiply (Move.sInterpret m) ls.postMove 
      (ls', highestLevel') =
        case ls.endState of
          End wl Havent    -> ({ls | endState <- End wl Have}, s.highestLevel)
          End (Win _) Have -> (ls, s.highestLevel)
          _                ->
            if | allTogether postMove' ->
                let movesLeft = ls.movesLeft - 1 in
                ( { endState  =
                      End (Win {pre=ls.postMove, move=m, movesLeft=movesLeft})
                        Havent
                  , movesLeft = movesLeft
                  , postMove  = postMove'
                  , preMove   = ls.postMove
                  }
                , s.currLevelIndex + 1)
               | ls.movesLeft == 1      -> 
                 let s0 = (initialLevelState s.currLevel)
                     lose =
                       { pre      = ls.postMove
                       , move     = m
                       , maxMoves = s.currLevel.maxMoves
                       , init     = s.currLevel.initial
                      }
                 in
                 ({s0 | endState <- End (Lose lose) Havent}, s.highestLevel)
               | otherwise             ->
                 ( { movesLeft = ls.movesLeft - 1
                   , postMove  = postMove'
                   , preMove   = ls.postMove
                   , endState  = Normal
                   }
                 , s.highestLevel)
  in
  { s | levelState <- ls'
  , lastMove <- Just m
  , highestLevel <- max s.highestLevel highestLevel'
  }


run setHighestLevel setLocalStorageChan g =
  let updates =
        Signal.mergeMany
        [ filterMap (Maybe.map Clicked) NoOp (Signal.subscribe clickMoveChan)
        , Signal.map (\_ -> NextLevel) (Signal.subscribe nextLevelChan)
        , Signal.map SetEndState (Signal.subscribe setEndStateChan)
        , Signal.map (\_ -> ResetLevel) (Signal.subscribe resetLevelChan)
        , Signal.map SetLevel (Signal.subscribe setLevelChan)
        , Signal.map SetHighestLevel setHighestLevel
        ]

      state = Signal.foldp update (initialGameState g) updates

      openingScreen = let l = g ! 0 in Draw.plane {currTranses=l.initial, movesLeft=l.maxMoves}
      stages        = Draw.animations openingScreen updates state
      buttons       =
        Signal.map (
          color Color.white
          << Html.toElement w buttonsHeight
          << Draw.transButtons
          << .currLevel)
          state
    
      hovers = 
        Signal.subscribe hoverMoveChan
        |> Signal.dropRepeats
        |> Signal.merge (Signal.map (\_ -> Nothing)
            (Signal.subscribe clickMoveChan))
        |> Signal.map2 (\s x -> case s.levelState.endState of {
             Normal -> x; _ -> Nothing}) state

      ends_ = Draw.loseAnimEnds state
      sets_ = sendSets setLocalStorageChan state

      hoverOverlay : Signal Element
      hoverOverlay =
        Signal.map2 (\mm s -> maybe empty (Draw.hoverArt s.levelState) mm)
          hovers state

      gameMode = Signal.foldp (\_ _ -> PlayLevel) TitleScreen (Signal.subscribe startGameChan)
  in
  signalMap6 (\mode mainScreen hov butts s (winW, winH) ->
    let screen =
          case mode of
            TitleScreen -> Draw.titleScreen
            PlayLevel ->
              flow inward
              [ let lbs = Draw.levelButtons s in
                container w (heightOf lbs) (topRightAt (absolute 0) (absolute 3)) lbs
              , flow down
                [ flow inward
                  [ hov
                  , mainScreen
                  , collage w h [filled Color.white (rect w h)]
                  ]
                , butts
                ]
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
    state
    Window.dimensions

wrapWithClass c elt =
  Html.toElement (widthOf elt) (heightOf elt) (Html.div [class c] [Html.fromElement elt])

pad k e = container (widthOf e + 2*k) (heightOf e + 2*k) middle e

bordered r c elt =
  color c (container (widthOf elt + 2*r) (heightOf elt + 2*r) middle elt)

sendSets setLocalStorageChan state =
  Signal.foldp max 0 (Signal.map .highestLevel state)
  |> Signal.dropRepeats
  |> Signal.map (Signal.send setLocalStorageChan)
  |> Native.Execute.schedule

