module Game.Event where

import Game.UIEvent
import Game.ComputeEvent

data Event =
    Ui UIEvent |
    Compute ComputeEvent
    
