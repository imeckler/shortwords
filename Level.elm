module Level where

import Array
import Native.Execute
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
import Signal.Extra as Signal
import Maybe
import Random
import Generate

allTogether (t1::ts) =
  let closeEnough t2 = distTransform2D t1 t2 < 0.01 in
  List.all closeEnough ts

-- Pass a dummy level
initialGameState : Level -> GameState
initialGameState lev =
  { levelState = initialLevelState lev
  , currLevel  = lev
  , lastMove   = Nothing
  , totalScore = 0
  , seed       = Random.initialSeed 10
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
  PlayLevelOfDifficulty d -> playLevelOfDifficulty d s
  Clicked m               -> updateWithMove m s
  Hovered _               -> s
  Unhovered               -> s
  SetTotalScore n         -> {s | totalScore <- n}
  SetEndState es          -> let ls = s.levelState in { s | levelState <- { ls | endState <- es } }
  ResetLevel              -> {s | levelState <- initialLevelState s.currLevel}
  NoOp                    -> s

playLevelOfDifficulty d s =
  let (lev, seed') = Random.generate (Generate.randomLevel d) s.seed in
  { s
  | levelState <- initialLevelState lev
  , currLevel  <- lev
  , lastMove   <- Nothing
  , seed       <- seed'
  }

updateWithMove : Move -> GameState -> GameState
updateWithMove m s =
  let ls = s.levelState
      postMove' = Move.sMultiply (Move.sInterpret m) ls.postMove 
      (ls', scoreIncr) =
        case ls.endState of
          End wl Havent    -> ({ls | endState <- End wl Have}, 0)
          End (Win _) Have -> (ls, 0)
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
                , difficultyScore s.currLevel.difficulty)
               | ls.movesLeft == 1      -> 
                 let s0 = (initialLevelState s.currLevel)
                     lose =
                       { pre      = ls.postMove
                       , move     = m
                       , maxMoves = s.currLevel.maxMoves
                       , init     = s.currLevel.initial
                      }
                 in
                 ({s0 | endState <- End (Lose lose) Havent}, 0)
               | otherwise             ->
                 ( { movesLeft = ls.movesLeft - 1
                   , postMove  = postMove'
                   , preMove   = ls.postMove
                   , endState  = Normal
                   }
                 , 0)
  in
  { s | levelState <- ls'
  , lastMove       <- Just m
  , totalScore     <- s.totalScore + scoreIncr
  }


run setTotalScore setLocalStorageChan =
  let dummyLevel = { availableMoves = [], maxMoves = 0, initial = [], difficulty = S }
      updates =
        Signal.mergeMany
        [ filterMap (Maybe.map Clicked) NoOp (Signal.subscribe clickMoveChan)
        , Signal.map PlayLevelOfDifficulty (Signal.subscribe playLevelOfDifficultyChan)
        , Signal.map SetEndState (Signal.subscribe setEndStateChan)
        , Signal.map (\_ -> ResetLevel) (Signal.subscribe resetLevelChan)
        , Signal.map SetTotalScore setTotalScore
        ]

      state = Signal.foldp update (initialGameState dummyLevel) updates
      openingScreen = Draw.chooseDifficultyScreen 0 -- let l = g ! 0 in Draw.plane {currTranses=l.initial, movesLeft=l.maxMoves}
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
            ChooseLevel -> Draw.chooseDifficultyScreen s.totalScore
            PlayLevel   ->
              flow inward
              [ let dbs = Draw.difficultyButtonsOverlay in
                container w (heightOf dbs) (topRightAt (absolute 0) (absolute 3)) dbs
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
  Signal.foldp' max identity (Signal.map .totalScore state)
  |> Signal.dropRepeats
  |> Signal.map (Signal.send setLocalStorageChan)
  |> Native.Execute.schedule

