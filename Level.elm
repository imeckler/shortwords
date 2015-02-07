module Level where

import Debug
import Inputs(..)
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
import Stage(Stage, Forever, ForATime)
import Stage.Infix(..)
import List
import Easing (ease, float, easeInQuad, easeOutQuad)
import Signal
import Maybe

tuply : Transform2D -> (Float, Float, Float, Float, Float, Float)
tuply = Native.IsomUtil.tuply

winning : Level -> Move.SInterp -> Bool
winning lev trans = 
  let closeEnough trans goal =
    let (g0,g1,g2,g3,g4,g5) = tuply goal
        (t0,t1,t2,t3,t4,t5) = tuply trans
        d = 
          List.map2 (\g t -> (g - t)^2)
            [g0,g1,g2,g3,g4,g5] [t0,t1,t2,t3,t4,t5]
          |> List.sum
    in
    d < 0.01
  in
  and <| List.map2 closeEnough trans lev.goal


w = 500
h = 500

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

transStages : Element -> Signal Update -> Signal GameState -> Signal (Stage Forever Element)
transStages openingScreen updates state =
  Signal.map2 (\u s -> case u of
    Clicked m -> 
      if s.levelState.hasWon then Nothing else
        Just <| Stage.sustain <| 
          Stage.map 
            (\t -> plane {currTranses=t, movesLeft=s.levelState.movesLeft})
            (Move.interpret m s.levelState.preMove)

    NextLevel -> Just <| Stage.stayForever <|
      plane {currTranses=s.currLevel.initial, movesLeft=s.currLevel.maxMoves}

    _         -> Nothing)
    updates (Signal.sampleOn updates state)
  |> filterJust (Stage.stayForever openingScreen)

winStages openingScreen state = 
  filterMap (\s ->
    if s.levelState.hasWon
    then Maybe.map (\m -> winAnim m s) s.lastMove
    else Nothing)
    (Stage.stayForever openingScreen) state

update : Update -> GameState -> GameState
update u s = case u of
  NextLevel -> updateNextLevel s
  Clicked m -> updateWithMove m s
  Hovered _ -> s
  Unhovered -> s
  NoOp      -> s

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
        case ls.winState of
          End _ Have    -> ls
          End wl Havent -> {ls | endState <- End wl Have}
          Normal        ->
            if | winning s.currLevel postMove' ->
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
        ]
        |> Signal.map (Debug.watch "updates")

      state         = Signal.foldp update (initialGameState g) updates
      openingScreen = let l = List.head g in plane {currTranses=l.initial, movesLeft=l.maxMoves}
      stages        = Signal.mergeMany
        [ transStages openingScreen updates state, winStages openingScreen state ]
      buttons = Signal.map (Html.toElement w 200 << transButtons << .currLevel) state
  in
  Signal.map2 (\mainScreen butts -> flow down [ mainScreen, butts ])
    (Stage.run stages (Time.every 30)) buttons


{-

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
-}
