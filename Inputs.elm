module Inputs where

import Signal
import GameTypes(..)
import Move(Move)

moveClicks = Signal.subscribe clickMoveChan

clickMoveChan : Signal.Channel (Maybe Move)
clickMoveChan = Signal.channel Nothing

hoverMoveChan : Signal.Channel (Maybe Move)
hoverMoveChan = Signal.channel Nothing

nextLevelChan : Signal.Channel ()
nextLevelChan = Signal.channel ()

startGameChan = Signal.channel ()

setEndStateChan : Signal.Channel EndState
setEndStateChan = Signal.channel Normal

resetLevelChan : Signal.Channel ()
resetLevelChan = Signal.channel ()

setLevelChan : Signal.Channel Int
setLevelChan = Signal.channel 0
