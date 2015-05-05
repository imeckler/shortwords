module Inputs where

import Signal
import GameTypes exposing (..)
import Move exposing (Move)

moveClicks = clickMoveChan.signal

clickMoveChan : Signal.Mailbox (Maybe Move)
clickMoveChan = Signal.mailbox Nothing

hoverMoveChan : Signal.Mailbox (Maybe Move)
hoverMoveChan = Signal.mailbox Nothing

playLevelOfDifficultyChan : Signal.Mailbox Difficulty
playLevelOfDifficultyChan = Signal.mailbox S

startGameChan = Signal.mailbox ()

setEndStateChan : Signal.Mailbox EndState
setEndStateChan = Signal.mailbox Normal

resetLevelChan : Signal.Mailbox ()
resetLevelChan = Signal.mailbox ()

backToMenuChan : Signal.Mailbox ()
backToMenuChan = Signal.mailbox ()
