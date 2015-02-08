module Inputs where

import Signal
import Move(Move)

moveClicks = Signal.subscribe clickMoveChan

clickMoveChan : Signal.Channel (Maybe Move)
clickMoveChan = Signal.channel Nothing

hoverMoveChan : Signal.Channel (Maybe Move)
hoverMoveChan = Signal.channel Nothing

nextLevelChan : Signal.Channel ()
nextLevelChan = Signal.channel ()

startGameChan = Signal.channel ()
