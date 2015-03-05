module Inputs where

import Signal
import GameTypes(..)
import Move(Move)

moveClicks = Signal.subscribe clickMoveChan

clickMoveChan : Signal.Channel (Maybe Move)
clickMoveChan = Signal.channel Nothing

hoverMoveChan : Signal.Channel (Maybe Move)
hoverMoveChan = Signal.channel Nothing

playLevelOfDifficultyChan : Signal.Channel Difficulty
playLevelOfDifficultyChan = Signal.channel S

startGameChan = Signal.channel ()

setEndStateChan : Signal.Channel EndState
setEndStateChan = Signal.channel Normal

resetLevelChan : Signal.Channel ()
resetLevelChan = Signal.channel ()

backToMenuChan : Signal.Channel ()
backToMenuChan = Signal.channel ()
