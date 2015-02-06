module Level where

import GameTypes(..)
import Window
import Util(..)
import Move
import Move(Move)
import Draw(..)
import Graphics.Element(..)
import Graphics.Collage(..)
import Transform2D(Transform2D)
import Transform2D
import Html
import Html.Events(..)
import Native.IsomUtil
import Text
import Color
import Time (second)
import Time
import Stage
import Stage.Infix(..)
import List
import Easing (ease, float, easeInQuad, easeOutQuad)
import Signal
import Maybe

type Update
  = Clicked Move
  | NextLevel
  | Hovered Move

tuply : Transform2D -> (Float, Float, Float, Float, Float, Float)
tuply = Native.IsomUtil.tuply

winning lev trans = 
  let (g0,g1,g2,g3,g4,g5) = tuply lev.goal
      (t0,t1,t2,t3,t4,t5) = tuply trans
      d = 
        List.map2 (\g t -> (g - t)^2)
          [g0,g1,g2,g3,g4,g5] [t0,t1,t2,t3,t4,t5]
        |> List.sum
  in
  d < 0.01

w = 500
h = 500

initialGameState : Game -> GameState
initialGameState (lev::levs) = 
  { levelState = initialLevelState lev
  , currLevel  = lev
  , rest       = levs
  , finished   = False
  }

initialLevelState : Level -> LevelState
initialLevelState lev =
  { movesLeft = lev.maxMoves
  , postMove  = lev.initial
  , preMove   = lev.initial
  , hasWon    = False
  , justLost  = False
  }

transes updates state =
  let init = Stage.stayForever lev.initial in
  Signal.map2 (\u s -> case u of
    Clicked m -> Just <| Stage.sustain <| Move.interpret m s.preMove
    _         -> Nothing)
    updates state
  |> filterJust
  |> (\ss -> Stage.run ss (Time.every 30))

clickMoves = Signal.subscribe clickMoveChan


update u s = case u of
  NextLevel -> updateNextLevel s
  Clicked m -> updateWithMove m s

updateNextLevel s =
  case s.rest of
    []      -> { s | finished <- True }
    l :: ls -> { levelState = initialLevelState l, currLevel = l, rest = ls }

updateWithMove m s =
  let ls = s.levelState
      postMove' = Move.sMultiply (Move.sInterpret m) ls.postMove 
      ls' =
        if | s.hasWon -> s
           | winning g.currLevel postMove' ->
            { hasWon    = True
            , movesLeft = ls.movesLeft - 1
            , postMove  = postMove'
            , preMove   = ls.postMove
            , justLost  = False
            }
          | ls.movesLeft == 1      -> 
            let s0 = (initialLevelState g.currLevel) in {s0 | justLost <- True}
          | otherwise             ->
            { movesLeft = ls.movesLeft - 1
            , postMove  = postMove'
            , preMove   = ls.postMove
            , hasWon    = False
            , justLost  = False
            }
  in
  { s | levelState <- ls' }


run g =

  let updates =
        Signal.mergeMany
        [ List.filterMap (Maybe.map Clicked) (Signal.subscribe clickMoveChan)
        , Signal.map (\_ -> NextLevel) (Signal.subscribe nextLevelChan)
        , Signal.map Hovered (Signal.subscribe hoverMoveChan)
        ]

      state = Signal.foldp update (initialState updates 
      transSig = transes updates state
      animState = Signal.map2 (\t s -> {currTranses = t, movesLeft = s.movesLeft}) transSig state 
  in
  plane 

run lev =
  let state       = filterFold (\mm s -> mm `Maybe.andThen` \m -> update lev m s) (initialState lev) moveClicks
      butts       = Html.toElement w 100 (transButtons lev)
      ghost       = goalGhost lev
  in
  Signal.map5 (\trans winScreen loseScreen s (winW, winH) ->
    container winW winH middle <|
    flow down
    [ collage w h
      [ axes
      , move (-200, 200) (movesLeft s)
      , ghost
      , groupTransform trans [defImage]
      , winScreen
      , loseScreen
      ]
      |> color backgroundColor
      |> withBorder 3 borderColor
    , butts
    ])
    (Stage.run (transes lev state) (Time.every 30))
    (Stage.run (winOverlay state) (Time.every 30))
    (Stage.run (loseOverlay state) (Time.every 30))
    state
    Window.dimensions

