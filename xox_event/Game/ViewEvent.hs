module Game.ViewEvent where

import qualified Game.World as World

data ViewEvent =
    End |
    PositionCursor Int Int |
    Redraw World.World
