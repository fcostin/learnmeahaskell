module Game.World where

import qualified Data.List as List
import qualified Game.Board as Game

type World = [Game.Move]

empty_world :: World
empty_world = []

apply_move :: Game.Move -> World -> World
apply_move m w =
    case m `List.elem` w of
        True -> w -- ignore duplicate moves
        False -> (m:w)

