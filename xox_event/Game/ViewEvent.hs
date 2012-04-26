module Game.ViewEvent where

import qualified Game.Board as Game

data ViewEvent =
    End |
    UpdateMove Game.Move |
    PositionCursor Int Int |
    Redraw
