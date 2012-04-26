module Game.GameEvent where

import qualified Game.Board as Game

data GameEvent =
    MakeMove Game.Move |
    GameFinish Game.Outcome
