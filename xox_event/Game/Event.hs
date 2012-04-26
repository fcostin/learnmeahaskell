module Game.Event where

import Game.UIEvent
import Game.GameEvent

data Event =
    UI UIEvent |
    Game GameEvent
    
